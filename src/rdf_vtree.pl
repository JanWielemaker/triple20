/*  File:    rdf_vtree.pl
    Author:  Jan Wielemaker
    Created: Jun 16 2003
    Purpose: Non-visual representation of an RDF hierarchy
*/

:- module(rdf_vtree, []).
:- use_module(library(pce)).
:- use_module(semweb(rdf_db)).
:- use_module(semweb(rdfs)).
:- use_module(particle).

:- pce_begin_class(rdf_vnode, dict_item, "Virtual node").

variable(children,	sheet*,       none, "Children by role").
variable(modified,      bool := @off, both, "Child-set is modified").
variable(rules,		[name],	      send, "Ruleset for expansion").
variable(generation,	int := 0,     get,  "RDF generation at create").
variable(seen,		bool := @on,  none, "Was I seen last update?").
variable(expand_status,	{none,partial,full} := none, get, "Expansion").

initialise(VN, Resource:name) :->
	"Create from resource"::
	rdfs_label(Resource, Label),
	send_super(VN, initialise, Resource, Label).

resource(VN, Resource:name) :<-
	get(VN, key, Resource).

role(VN, Role:name) :<-
	"Find the role (=visualization) we play"::
	(   get(VN, dict, Dict), Dict \== @nil
	->  get(Dict, role, Role)
	;   Role = rdf_root_node
	).

parent(VN, Parent:name) :<-
	"Get parent node"::
	get(VN, dict, Dict), Dict \== @nil,
	get(Dict, parent, Parent).

rules(VN, Rules:name) :<-
	"Ruleset for expansion"::
	(   get(VN, slot, rules, Rules),
	    Rules \== @default
	->  true
	;   get(VN, parent, Parent)
	->  get(Parent, rules, Rules)
	;   Rules = class_hierarchy
	).

gone(VN) :->
	"This node must be deleted"::
	(   get(VN, parent, Parent)
	->  get(Parent, slot, children, Sheet),
	    send(Sheet, for_some,
		 message(@arg1?value, delete, VN)),
	    send(Parent, modified, @on)
	;   ignore(send(VN, super_hyper, node, destroy))
	).


check_unseen_children(VN) :->
	"Set all children to seen := @off"::
	(   get(VN, slot, children, Sheet),
	    Sheet \== @nil
	->  send(Sheet, for_all,
		 message(@arg1?value, for_all,
			 if(?(@arg1, slot, seen) == @on,
			    message(@arg1, slot, seen, @off),
			    message(@arg1, gone))))
	;   true
	).


add_child(VN, Child:name, Role:name, ChildNode:rdf_vnode) :<-
	"Add a child, given a specified role"::
	(   get(VN, slot, children, Sheet),
	    Sheet \== @nil
	->  true
	;   send(VN, slot, children, new(Sheet, sheet))
	),
	(   get(Sheet, value, Role, Set)
	->  true
	;   send(Sheet, value, Role, new(Set, rdf_vnodeset(VN, Role)))
	),
	(   get(Set, member, Child, ChildNode)
	->  send(ChildNode, slot, seen, @on)
	;   send(Set, append, new(ChildNode, rdf_vnode(Child))),
	    send(VN, modified, @on)
	).

expand(VN) :->
	"Generate my siblings"::
	get(VN, resource, Resource),
	get(VN, role, Role),
	get(VN, rules, Rules),
	(   Rules::child(Resource, Role, Child, ChildRole),
	    get(VN, add_child, Child, ChildRole, _ChildNode),
	    fail
	;   true
	),
	send(VN, check_unseen_children),
	send(VN, slot, expand_status, full),
	rdf_generation(G),
	send(VN, slot, generation, G),
	(   get(VN, modified, @on)
	->  send(VN, sort_childs)
	;   true
	).

sort_childs(VN) :->
	"Sort the childs by <-label"::
	(   get(VN, slot, children, Sheet),
	    Sheet \== @nil
	->  send(Sheet, for_all, message(@arg1?value, sort))
	;   true
	).

update(VN) :->
	"Update if I'm fully expanded"::
	(   get(VN, expand_status, full),
	    get(VN, generation, G),
	    rdf_generation(G)
	->  true
	;   send(VN, expand)
	).

children(VN, Children:sheet) :<-
	"Get children, update if necessary"::
	(   \+ get(VN, expand_status, full)
	->  send(VN, expand)
	;   true
	),
	get(VN, slot, children, Children),
	Children \== @nil.

can_expand(VN) :->
	"Test if there are children"::
	(   get(VN, slot, children, Children),
	    Children \== @nil,
	    get(Children?members, size, N),
	    N > 0
	;   get(VN, resource, Resource),
	    get(VN, role, Role),
	    get(VN, rules, Rules),
	    Rules::child(Resource, Role, _Child, _ChildRole)
	->  true
	).

child(VN, Child:name, Create:[bool], ChildNode:rdf_vnode) :<-
	(   get(VN, slot, children, Sheet),
	    Sheet \== @nil,
	    get(Sheet?members, find,
		message(@arg1?value, member, Child), Attr)
	->  get(Attr, value, Set),
	    get(Set, member, Child, ChildNode)
	;   Create \== @off,
	    \+ get(VN, expand_status, full),
	    get(VN, resource, Resource),
	    get(VN, role, Role),
	    get(VN, rules, Rules),
	    Rules::child(Resource, Role, Child, ChildRole)
	->  get(VN, add_child, Child, ChildRole, ChildNode),
	    send(VN, slot, expand_status, partial)
	).
	    

:- pce_end_class(rdf_vnode).


:- pce_begin_class(rdf_vnodeset, dict, "Set of nodes").

variable(role,	 name,      get, "Roles played by this node").
variable(parent, rdf_vnode, get, "Parent node").

initialise(NS, Parent:rdf_vnode, Role:name) :->
	send_super(NS, initialise),
	send(NS, slot, parent, Parent),
	send(NS, slot, role, Role).

:- pce_end_class(rdf_vnodeset).

