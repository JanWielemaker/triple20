/*  File:    tree.pl
    Author:  Jan Wielemaker
    Created: Jun  3 2003
    Purpose: 
*/

:- use_module(rdf_db).
:- use_module(rdfs).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Define hierarchy visualisation.

	* A node is identified by a resource and a `role'.  The role
	  defines the visual appearance as well as how the node is
	  expanded.  Roles are realised by an XPCE class.  Defined
	  roles are:

	  	class
		abstract_class
		property
		individual
		orphans

	  Visualisations are created using

	        create_visual(Visual, Resource, Role, Parent)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

resource(class, image, image('16x16/class.xpm')).

		 /*******************************
		 *	     HIERARCHY		*
		 *******************************/

:- pce_begin_class(rdf_hierarchy, tree,
		   "Display an RDF hierarchy").

variable(rules,	name := class_hierarchy, get,
	 "Rule-set that defines expansion").
variable(resources, chain_table, get,
	 "Provide indexed access to displayed resources").
variable(show_namespace, bool := @on, get,  "Show namespaces").

:- pce_global(@onto_tree_recogniser,
	      make_onto_tree_recogniser).

make_onto_tree_recogniser(R) :-
	new(R, handler_group),
	send(R, append, click_gesture(left, '', single,
				      message(@receiver, on_left_click))),
	send(R, append, new(KB, key_binding)),
	send(KB, function, '\\ef', find).

initialise(H, RuleSet:[name]) :->
	send_super(H, initialise),
	(   RuleSet == @default
	->  true
	;   send(H, slot, rules, RuleSet)
	),
	send(OT, direction, list),
	send(OT, level_gap, 20),
	send(H, create_root).

create_root(H) :->
	"Create the root-node"::
	send(H, clear),
	get(H, rules, Rules),
	Rules:root(Resource, Role),
	create_node(H, Resource, Role, Node),
	send(H, root, Node).

node_label(OT, Id:name, Label:name) :<-
	"Get label to display for Id"::
	(   get(OT, show_namespace, @off)
	->  rdfs_label(Id, Label)
	;   rdfs_ns_label(Id, Label)
	).

:- pce_group(nodes).

register(OT, N:rdf_node) :->
	"Register in <-resources table"::
	get(OT, resources, Table),
	get(N, resource, Resource),
	send(Table, append, N, Resource).

unregister(OT, N:rdf_node) :->
	"Register in <-resources table"::
	get(OT, resources, Table),
	get(N, resource, Resource),
	send(Table, delete, N, Resource).

:- pce_group(event).

event(OT, Ev:event) :->
	"Deal with events"::
	(   send_super(OT, event, Ev)
	;   send(@onto_tree_recogniser, event, Ev)
	).

on_left_click(OT) :->
	"Deselect all nodes"::
	send(OT, selection, @nil).

selected(OT, Node:rdfs_node) :->
	"User selected a node"::
	send(OT, selection, Node?image),
	(   get(OT, message, M),
	    M \== @nil
	->  get(Node, id, Term),
	    send(M, forward, Term)	% @arg1 = term
	;   true
	).

open_node(OT, Node:rdfs_node) :->
	"User double-clicked a node"::
	(   get(OT, open_message, M),
	    M \== @nil
	->  get(Node, id, Term),
	    send(M, forward, Term)	% @arg1 = term
	;   true
	).

:- pce_end_class(rdf_hierarchy).


		 /*******************************
		 *	       NODES		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Keep track of a query.  The job is to:

	* Quicky re-run te query and check what is changed
	* 

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(rdf_vnode, dict_item, "Virtual node").

variable(children,	sheet*,       get,  "Children by role").
variable(modified,      bool := @off, both, "Child-set is modified").

initialise(VN, Resource:name) :->
	"Create from resource"::
	rdfs_ns_label(Resource, Label),
	send_super(VN, initialise, Resource, Label).

child(VN, Child:name, Role:name) :->
	(   get(VN, children, Sheet),
	    Sheet \== @nil
	->  true
	;   send(VN, slot, children, new(Sheet, sheet))
	),
	(   get(Sheet, value, Role, Set)
	->  true
	;   send(Sheet, value, Role, new(Set, rdf_vnodeset))
	),
	(   send(Set, member, Child)
	->  true
	;   send(Set, append, rdf_vnode(Child)),
	    send(VN, modified, @on)
	).

:- pce_end_class(rdf_vnode).


:- pce_begin_class(rdf_vnodeset, dict, "Set of nodes").

variable(role,	name,  get, "Roles played by this node").
variable(show,  [int], get, "# nodes displayed").

initialise(NS, Role:name) :->
	send_super(NS, initialise),
	send(NS, slot, role, Role).

:- pce_end_class(rdf_vnodeset).
















:- pce_begin_class(rdf_node, node,
		   "Node of an RDF hierarchy").

variable(resource,	name, 	get, "Represented resource").
variable(icon,		image*, get, "Prepresented icon").
variable(by_role,	sheet*, get, "Childs by role").

class_variable(icon, image*, resource(class)).

initialise(N, Resource:name, Tree:rdf_hierarchy) :->
	get(Tree, node_label, Resource, Label),
	send(N, slot, resource, Resource),
	send_super(N, initialise, new(D, device)),
	send(D, format, @rdf_node_format),
	(   get(D, icon, Icon), Icon \== @nil
	->  send(D, display, bitmap(Icon))
	;   true
	),
	send(D, display, text(Label)),
	send(Tree, register, N).

unlink(N) :->
	(   get(N, tree, Tree),
	    Tree \== @nil
	->  send(Tree, unregister, N)
	;   true
	),
	send_super(N, unlink).

can_expand(N) :->
	get(N, resource, Resource),
	get(N, role, Role),
	get(N, rules, Rules),
	Rules:child(Resource, Role, _Child, _SR), !.

expand(N) :->
	get(N, resource, Resource),
	get(N, role, Role),
	get(N, rules, Rules),
	send(N, slot, by_role, new(ByRole, sheet)),
	(   Rules:child(Resource, Role, Child, SR),
	    (	get(ByRole, value, SR, Set)
	    ->	send(Set, append, Child)
	    ;	send(ByRole, value, SR, chain(Child))
	    ),
	    fail
	;   true
	).
	
	


:- pce_end_class(rdf_node).

