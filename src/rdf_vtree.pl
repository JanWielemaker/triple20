/*  File:    rdf_vtree.pl
    Author:  Jan Wielemaker
    Created: Jun 16 2003
    Purpose: Non-visual representation of an RDF hierarchy
*/

:- module(rdf_vtree, []).
:- use_module(library(pce)).
:- use_module(semweb(rdf_db)).
:- use_module(semweb(rdfs)).
:- use_module(library(particle)).

:- pce_begin_class(rdf_vnode, dict_item, "Virtual node").

variable(children,	sheet*,       none, "Children by role").
variable(modified,      bool := @off, both, "Child-set is modified").
variable(rules,		[name],	      send, "Ruleset for expansion").
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
	->  true
	;   send(Set, append, new(ChildNode, rdf_vnode(Child))),
	    send(VN, modified, @on)
	).

expand(VN) :->
	"Generate my siblings"::
	get(VN, resource, Resource),
	get(VN, role, Role),
	get(VN, rules, Rules),
	(   Rules:child(Resource, Role, Child, ChildRole),
	    get(VN, add_child, Child, ChildRole, _ChildNode),
	    fail
	;   true
	),
	send(VN, sort_childs),
	send(VN, slot, expand_status, full).

sort_childs(VN) :->
	"Sort the childs by <-label"::
	(   get(VN, slot, children, Sheet),
	    Sheet \== @nil
	->  send(Sheet, for_all, message(@arg1?value, sort))
	;   true
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
	    Rules:child(Resource, Role, _Child, _ChildRole)
	->  true
	).

child(VN, Child:name, ChildNode:rdf_vnode) :<-
	(   get(VN, slot, children, Sheet),
	    Sheet \== @nil,
	    get(Sheet?members, find,
		message(@arg1?value, member, Child), Attr)
	->  get(Attr, value, Set),
	    get(Set, member, Child, ChildNode)
	;   \+ get(VN, expand_status, full),
	    get(VN, resource, Resource),
	    get(VN, role, Role),
	    get(VN, rules, Rules),
	    Rules:child(Resource, Role, Child, ChildRole)
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


		 /*******************************
		 *	      RULESETS		*
		 *******************************/

:- begin_particle(class_hierarchy, []).
:- use_module(semweb(rdfs)).

child(Resource, Role, Child, SubRole) :-
	rdfs_individual_of(Resource, rdfs:'Class'),
	isa_class(Role, rdf_class_node),
	(   rdf_has(Child, rdfs:subClassOf, Resource),
	    (	rdfs_subclass_of(Child, rdfs:'Class')
	    ->	SubRole = rdf_metaclass_node
	    ;	SubRole = Role
	    )
	;   rdf_has(Child, rdf:type, Resource),
	    (	rdfs_individual_of(Child, rdf:'Property')
	    ->	SubRole = rdf_property_node
	    ;	rdfs_individual_of(Child, rdf:'List')
	    ->	\+ rdf_has(_, rdf:rest, Child),
	        SubRole = rdf_list_node
	    ;	SubRole = rdf_individual_node
	    )
	;   rdf_equal(Resource, rdfs:'Resource'),
	    orphan_class(_),
	    Child = '<Orphan Classes>',
	    SubRole = rdf_orphan_node
	).
child(Resource, Role, Child, Role) :-
	isa_class(Role, rdf_property_node),
	rdf_has(Child, rdfs:subPropertyOf, Resource).
child(Resource, Role, Child, SubRole) :-
	isa_class(Role, rdf_list_node),
	SubRole = rdf_list_member_node,
	rdfs_member(Child, Resource).
child('<Orphan Classes>', rdf_orphan_node, Orphan, rdf_class_node) :-
	orphan_class(Orphan).

orphan_class(Orphan) :-
	rdfs_individual_of(Orphan, rdfs:'Class'),
	\+ rdf_has(Orphan, rdfs:subClassOf, _),
	\+ rdf_equal(Orphan, rdfs:'Resource').

parent('<Orphan Classes>', Parent) :- !,
	rdf_equal(Parent, rdfs:'Resource').
parent('<Untyped individuals>', Parent) :- !,
	rdf_equal(Parent, rdfs:'Resource').
parent(Resource, Parent) :-
	(   rdfs_individual_of(Resource, rdfs:'Class')
	->  (   rdf(Resource, rdfs:subClassOf, Parent)
	    ->  true
	    ;   Parent = '<Orphan Classes>'
	    )
	;   rdfs_individual_of(Resource, rdf:'Property'),
	    rdf_has(Resource, rdfs:subPropertyOf, Parent)
	->  true
	;   (   rdf(Resource, rdf:type, Parent)
	    ->	true
	    ;	Parent = '<Untyped individuals>'
	    )
	).

:- end_particle.
