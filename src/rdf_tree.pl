/*  File:    tree.pl
    Author:  Jan Wielemaker
    Created: Jun  3 2003
    Purpose: Visualise RDF hierarchy
*/

:- module(rdf_tree, []).
:- use_module(library(pce)).
:- use_module(rdf_vtree).
:- use_module(semweb(rdf_db)).
:- use_module(semweb(rdfs)).

:- pce_autoload(rdf_explorer, rdf_explorer).
:- pce_autoload(partof_hyper, library(hyper)).

resource(class,       image, image('16x16/class.xpm')).
resource(metaclass,   image, image('16x16/Metaclass.gif')).
resource(orphanclass, image, image('16x16/orphanclass.xpm')).
resource(individual,  image, image('16x16/Instance.gif')).
resource(property,    image, image('16x16/SlotDirect.gif')).
resource(list,        image, image('16x16/list.xpm')).
resource(list_member, image, image('16x16/list_member.xpm')).

		 /*******************************
		 *	     HIERARCHY		*
		 *******************************/

:- pce_begin_class(rdf_tree, tree,
		   "Display an RDF hierarchy").

variable(vtree,          rdf_vnode,   get, "Underlying data").
variable(show_namespace, bool := @on, get, "Add namespace indicator to label").
variable(selectable,	 chain*,      both, "Roles of selectable nodes").
variable(message,	 code*,	      both, "Message on select").
variable(open_message,	 code*,	      both, "Message  on double-click").

:- pce_global(@onto_tree_recogniser,
	      make_onto_tree_recogniser).

make_onto_tree_recogniser(R) :-
	new(R, handler_group),
	send(R, append, click_gesture(left, '', single,
				      message(@receiver, on_left_click))),
	send(R, append, new(KB, key_binding)),
	send(KB, function, '\\ef', find).

initialise(H, Root:[name], RuleSet:[name]) :->
	send_super(H, initialise),
	(   Root == @default
	->  rdf_equal(rdfs:'Resource', TheRoot)
	;   TheRoot = Root
	),
	send(H, slot, vtree, new(N, rdf_vnode(TheRoot))),
	send(N, rules, RuleSet),
	send(H, direction, list),
	send(H, level_gap, 20),
	send(H, create_root).

create_root(H) :->
	"Create the root-node"::
	send(H, clear),
	get(H, vtree, VNode),
	get(H, create_node, VNode, Node),
	send(H, root, Node).

expand_root(H) :->
	"Expand the root node"::
	get(H, root, Root),
	send(Root, collapsed, @off).

create_node(H, V:rdf_vnode, N:rdf_node) :<-
	"Create a real node from a virtual one"::
	get(V, role, Role),
	Term =.. [Role, V, H],
	new(N, Term).

node_label(OT, Id:name, Label:name) :<-
	"Get label to display for Id"::
	(   get(OT, show_namespace, @off)
	->  rdfs_label(Id, Label)
	;   rdfs_ns_label(Id, Label)
	).

:- pce_group(build).

member(OT, Id:name, Node:rdf_node) :<-
	"Find Node representing ID"::
	get(OT?root, find, @arg1?resource == Id, Node).

add(OT, Id:name, Role:[name]) :->
	"Add node for Id to the tree, as well as the path"::
	get(OT, add, Id, Role, _Node).

add(OT, Resource:name, _Role:[name], Node:rdf_node) :<-
	"Expand the tree to show a resource"::
	(   get(OT, member, Resource, Node)
	->  true
	;   get(OT?root, virtual, VN),
	    get(VN, rules, Rules),
	    get(VN, resource, Root),
	    findall(Path, path(Resource, Root, Rules, Path), [P0|_Paths]),
	    display_path(P0, OT, Node)
	).
	
path(Resource, Resource, _, [Resource]) :- !.
path(Resource, Root, Rules, [Resource|T]) :-
	Rules:parent(Resource, Parent),
	path(Parent, Root, Rules, T).

display_path([H|_], OT, Node) :-
	get(OT, member, H, Node), !.
display_path([H|T], OT, Node) :-
	display_path(T, OT, Parent),
	get(Parent, virtual, VP),
	get(VP, child, H, VN),
	get(Parent?tree, create_node, VN, Node),
	send(Parent, son, Node),
	send_class(Parent, node, collapsed(@off)).


:- pce_group(event).

event(OT, Ev:event) :->
	"Deal with events"::
	(   send_super(OT, event, Ev)
	;   send(@onto_tree_recogniser, event, Ev)
	).

on_left_click(OT) :->
	"Deselect all nodes"::
	send(OT, selection, @nil).

selected(OT, Node:rdf_node) :->
	"User selected a node"::
	send(OT, selection, Node?image),
	(   get(OT, message, M),
	    M \== @nil
	->  get(Node, resource, Term),
	    send(M, forward, Term)	% @arg1 = term
	;   true
	).

open_node(OT, Node:rdf_node) :->
	"User double-clicked a node"::
	(   get(OT, open_message, M),
	    M \== @nil
	->  get(Node, resource, Term),
	    send(M, forward, Term)	% @arg1 = term
	;   true
	).

:- pce_group(diagram).

open_diagram(OT, Id:name) :->
	"Open triple diagram from resource Id"::
	get(OT, rdf_diagram, Diagram),
	send(Diagram, resource, Id),
	send(Diagram, expose).

rdf_diagram(OT, Diagram:rdf_explorer) :<-
	"Get associated RDF explorer"::
	(   get(OT, hypered, rdf_explorer, Diagram)
	->  true
	;   new(Diagram, rdf_explorer),
	    new(_, partof_hyper(OT, Diagram, rdf_explorer, hierarchy))
	).

:- pce_end_class(rdf_tree).


		 /*******************************
		 *	       NODES		*
		 *******************************/

:- pce_begin_class(rdf_node, node,
		   "Node of an RDF hierarchy").

variable(resource, name,   get,	"Represented resource").
variable(icon,	   image*, get,	"Prepresented icon").

class_variable(icon, image*, resource(class)).
class_variable(font, font,   normal).

:- pce_global(@rdf_node_format, make_rdf_node_format).
make_rdf_node_format(F) :-
	new(F, format(vertical, 1, @on)),
	send(F, row_sep, 5).

initialise(N, VN:rdf_vnode, Tree:rdf_tree) :->
	get(VN, resource, Resource),
	get(Tree, node_label, Resource, Label),
	send(N, slot, resource, Resource),
	send_super(N, initialise, new(D, device)),
	send(D, format, @rdf_node_format),
	(   get(N, icon, Icon), Icon \== @nil
	->  send(D, display, bitmap(Icon))
	;   true
	),
	get(N, font, Font),
	send(D, display, text(Label, font := Font)),
	new(_, hyper(N, VN, virtual, node)),
	(   send(N, can_expand)
	->  send_super(N, collapsed, @on)
	;   send_super(N, collapsed, @nil)
	).

virtual(N, VN:rdf_vnode) :<-
	get(N, hypered, virtual, VN).

:- pce_group(expand).

collapsed(N, V:bool*) :->
	(   V == @on
	->  send(N?sons, for_all, message(@arg1, delete_tree))
	;   send(N, expand)
	),
	send_super(N, collapsed, V).

can_expand(N) :->
	"Test whether this node has childs"::
	get(N, virtual, VN),
	send(VN, can_expand).

expand(N) :->
	"Expand this node"::
	get(N, virtual, VN),
	send(@display, busy_cursor),
	send(N?sons, for_all, message(@arg1, delete_tree)),
	send(VN?children, for_all,
	     message(N, expand_role, @arg1?name, @arg1?value, 10)),
	send(@display, busy_cursor, @nil).

expand_role(N, Role:name, Children:rdf_vnodeset, ShowMax:'[0..]') :->
	"Add sub-nodes for a role"::
	get(Children, members, Chain),
	(   ShowMax == @default
	->  send(Chain, for_all,
		 message(N, add_child, @arg1))
	;   new(Max, number(ShowMax)),
	    (   send(Chain, for_all,
		     and(message(Max, minus, 1),
			 message(Max, larger_equal, 0),
			 message(N, add_child, @arg1)))
	    ->  true
	    ;   get(Max, value, -1),
		get(Chain, size, Size),
		send(N, son, rdf_more_node(Role, Size, ShowMax))
	    )
	).

add_child(N, VN:rdf_vnode) :->
	"Create child for virtual node"::
	get(N?tree, create_node, VN, Son),
	send(N, son, Son).
	
show_more(N, MoreNode:rdf_more_node, Role:name, Count:int) :->
	"Show next Count nodes on Role"::
	get(N, tree, Tree),
	get(N, virtual, VN),
	get(VN?children, value, Role, Set),
	get(Set, members, Chain),
	get(MoreNode, here, Here),
	send(Chain, current_no, Here+1),
	(   next_member(Chain, Count, New),
	    get(Tree, create_node, New, Node),
	    send(N, son, Node, MoreNode),
	    fail
	;   true
	),
	(   get(Chain, current_no, NewHere),
	    NewHere \== 0		% For xpce =< 6.2.1
	->  send(MoreNode, here, NewHere-1)
	;   send(MoreNode, destroy)
	).
	
next_member(Chain, Max, Next) :-
	between(1, Max, _),
	get(Chain, next, Next).

:- pce_group(event).

:- free(@rdf_node_recogniser).
:- pce_global(@rdf_node_recogniser, make_rdf_node_recogniser).
:- pce_global(@rdf_node_popop, make_rdf_node_popup).

make_rdf_node_popup(Popup) :-
	Node = @arg1,
	new(Popup, popup(options)),
	send_list(Popup, append,
		  [ menu_item(show_id,
			      message(Node, report, inform, Node?resource)),
		    menu_item(copy_id_to_clipboard,
			      message(Node, copy)),
		    menu_item(copy_as_xml_identifier,
			      message(Node, copy, xml_identifier)),
		    menu_item(copy_as_xml_attribute,
			      message(Node, copy, xml_attribute)),
		    menu_item(view_rdf_source,
			      message(Node, view_rdf_source)),
		    menu_item(diagram_,
			      message(Node?tree, open_diagram, Node?resource)),
		    gap,
		    menu_item(delete,
			      message(Node, delete_resource))
		  ]).


make_rdf_node_recogniser(G) :-
	new(P, popup_gesture(@receiver?popup)),
	new(C1, click_gesture(left, '', single,
			      message(@receiver,
				      on_left_click))),
			      
	new(C2, click_gesture(left, '', double,
			      message(@receiver,
				      on_double_left_click))),
	new(G, handler_group(P, C1, C2)).
			      

popup(_, Popup:popup) :<-
	Popup = @rdf_node_popop.

event(N, Ev:event) :->
	"Handle node-event"::
	(   send_super(N, event, Ev)
	->  true
	;   send(Ev, post, N?image, @rdf_node_recogniser)
	).

on_left_click(N) :->
	"Select the current node"::
	send(N?tree, selected, N).

on_double_left_click(N) :->
	"Select the current node"::
	get(N, role, Role),
	get(N?tree, selectable, Roles),
	(   (	Roles == @nil
	    ;	send(Roles, member, Role)
	    )
	->  send(N?tree, open_node, N)
	;   send(N, report, warning, 'Cannot open %s class', Role)
	).

copy(N, As:[{resource,xml_identifier,xml_attribute}]) :->
	"Copy resource to clipboard"::
	get(N, resource, Resource),
	(   As == xml_identifier
	->  rdf_global_id(NS:Local, Resource),
	    new(Copy, string('%s:%s', NS, Local))
	;   As == xml_attribute
	->  rdf_global_id(NS:Local, Resource),
	    new(Copy, string('&%s;%s', NS, Local))
	;   Copy = Resource
	),
	send(@display, copy, Copy).

view_rdf_source(N) :->
	"Open Prolog editor on RDF source"::
	get(N, resource, Id),
	(   rdf_source_location(Id, File:Line)
	->  edit(file(File, line(Line)))
	;   send(N, report, warning, 'Cannot find source for %s', Id)
	).

:- pce_end_class(rdf_node).

:- pce_begin_class(rdf_class_node, rdf_node).
:- pce_end_class.

:- pce_begin_class(rdf_metaclass_node, rdf_class_node).
class_variable(icon, image*, resource(metaclass)).
:- pce_end_class.

:- pce_begin_class(rdf_root_node, rdf_class_node).
:- pce_end_class.

:- pce_begin_class(rdf_orphan_node, rdf_class_node).
class_variable(icon, image*, resource(orphanclass)).
class_variable(font, font, italic).
:- pce_end_class.

:- pce_begin_class(rdf_property_node, rdf_node).
class_variable(icon, image*, resource(property)).
:- pce_end_class.

:- pce_begin_class(rdf_individual_node, rdf_node).
class_variable(icon, image*, resource(individual)).
:- pce_end_class.

:- pce_begin_class(rdf_list_node, rdf_node).
class_variable(icon, image*, resource(list)).
:- pce_end_class.

:- pce_begin_class(rdf_list_member_node, rdf_node).
class_variable(icon, image*, resource(list_member)).
:- pce_end_class.


		 /*******************************
		 *	   SHOW MORE ...	*
		 *******************************/

:- pce_begin_class(rdf_more_node, node,
		   "Show more alternatives").

variable(role,	   name,  get, "Role to expand further").
variable(here,	   int,	  get, "Current location").
variable(size,	   int,	  get, "Total set-size").
variable(resource, name*, get, "Represented resource (@nil)").

initialise(N, Role:name, Size:int, Here:int) :->
	"Create `more' button"::
	send_super(N, initialise, new(D, figure)),
	send(N, slot, role, Role),
	send(N, slot, size, Size),
	send(N, slot, here, Here),
	send(D, pen, 1),
	send(D, border, 2),
	send(D, format, new(Fmt, format(vertical, 1, @on))),
	send(Fmt, adjustment, vector(center)),
	send(N, update_more),
	send(N, collapsed, @nil).

update_more(N) :->
	"Update	displayed buttons"::
	get(N, image, D),
	send(D, clear),
	send(D, display, text('Next', left, bold)),
	get(N, size, Size),
	get(N, here, Here),
	Left is Size - Here,
	(   Left < 10
	->  send(D, display, more_button(Left))
	;   Left < 100
	->  send(D, display, more_button(10)),
	    send(D, display, more_button(Left))
	;   Left < 1000
	->  send(D, display, more_button(10)),
	    send(D, display, more_button(100)),
	    send(D, display, more_button(Left))
	;   send(D, display, more_button(10)),
	    send(D, display, more_button(100)),
	    send(D, display, more_button(1000)),
	    send(D, display, text(string('(showing %d of %d) ', Here, Size), left, italic))
	).

update_label(_) :->
	"Dummy"::
	true.

update_expandable(_) :->
	"Dummy"::
	true.

refresh(_) :->
	"Dummy"::
	true.

here(N, Here:int) :->
	send(N, slot, here, Here),
	send(N, update_more).

more(N, More:[int]) :->
	"Show N more childs on this role"::
	get(N, parents, chain(Parent)),
	get(N, role, Role),
	send(Parent, show_more, N, Role, More).

:- pce_end_class(rdf_more_node).


		 /*******************************
		 *	    SMALL BUTTON	*
		 *******************************/

:- pce_begin_class(more_button, button,
		   "Button with minimal size").

class_variable(size, size, size(5,5)).

initialise(B, More:int) :->
	send_super(B, initialise, More,
		   message(B?device?node, more, More)),
	send(B, show_focus_border, @off).

:- pce_end_class(more_button).
