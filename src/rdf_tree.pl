/*  File:    tree.pl
    Author:  Jan Wielemaker
    Created: Jun  3 2003
    Purpose: Visualise RDF hierarchy
*/

:- module(rdf_tree, []).
:- use_module(library(pce)).
:- use_module(library(pce_unclip)).
:- use_module(semweb(rdf_db)).
:- use_module(semweb(rdfs)).
:- use_module(semweb(rdf_edit)).
:- use_module(particle).
:- use_module(rdf_template).
:- use_module(rdf_cache).
:- use_module(library(debug)).

:- pce_autoload(identifier_item, library(pce_identifier_item)).


		 /*******************************
		 *	     HIERARCHY		*
		 *******************************/

:- pce_begin_class(rdf_tree, tree,
		   "Display an RDF hierarchy").

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

initialise(H, Root:[name]) :->
	send_super(H, initialise),
	(   Root == @default
	->  rdf_equal(rdfs:'Resource', TheRoot)
	;   TheRoot = Root
	),
	send(H, direction, list),
	send(H, level_gap, 20),
	new(RootNode, rdf_node(TheRoot)),
%	call_rules(H, root_node(TheRoot, RootNode)),
	send(H, root, RootNode).

expand_root(H) :->
	"Expand the root node"::
	get(H, root, Root),
	send(Root, collapsed, @off).

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
	;   get(OT?root, resource, Root),
	    findall(Path, path(Resource, Root, OT, Path), [P0|_Paths]),
	    display_path(P0, OT, Node)
	).
	
path(Resource, Resource, _, [Resource-[]]) :- !.
path(Resource, Root, Tree, [Resource-Role|T]) :-
	call_rules(Tree, parent(Resource, Parent, Role)),
	path(Parent, Root, Tree, T).

display_path([H-Role|_], OT, Node) :-
	get(OT, member, H, Node),
	(   Role == []
	;   get(Node, class_name, Role)
	), !.
display_path([H-Role|T], OT, Node) :-
	display_path(T, OT, Parent),
	get(Parent, add_child, H, Role, Node),
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

arm(_, _:bool) :->
	fail.

:- pce_end_class(rdf_tree).


		 /*******************************
		 *	       NODES		*
		 *******************************/

:- pce_begin_class(rdf_node(resource), node,
		   "Node of an RDF hierarchy").

variable(resource, name, get,
	 "Represented resource").
variable(cache, int*, get,
	 "(Parent) cache that produced me").
variable(caches, sheet := new(sheet), get,
	 "Cached relations").

initialise(N, Resource:name) :->
	send(N, slot, resource, Resource),
	get(N, label, Label),
	send_super(N, initialise, Label),
	(   call_rules(N, child_cache(Resource, Cache, Role)),
	    send(N?caches, value, Role, Cache),
	    rdf_cache_attach(Cache, N),
	    fail
	;   true
	),
	send(N, update_can_expand).


unlink(N) :->
	send(N?caches, for_all,
	     message(@prolog, rdf_cache_detach, @arg1?value, N)),
	send_super(N, unlink).


label(N, Label:graphical) :<-
	get(N, resource, Resource),
	call_rules(N, label(Resource, Label)).


update_can_expand(N) :->
	"Update expansion-state"::
	(   send(N, can_expand)
	->  (   send(N?sons, empty)
	    ->	send_super(N, collapsed, @on)
	    ;	send_super(N, collapsed, @off)
	    )
	;   send_super(N, collapsed, @nil)
	).

:- pce_group(expand).

collapsed(N, V:bool*) :->
	(   V == @on
	->  send(N?sons, for_all, message(@arg1, delete_tree))
	;   send(@display, busy_cursor),
	    call_cleanup(send(N, expand),
			 send(@display, busy_cursor, @nil))
	),
	send_super(N, collapsed, V).

can_expand(N) :->
	"Test whether this node has childs"::
	get(N?caches?members, find,
	    message(N, can_expand_cache, @arg1?value),
	    _).

can_expand_cache(_N, Cache:int) :->
	\+ rdf_cache_empty(Cache).

expand(N) :->
	"Expand this node"::
	send(N?caches?members, for_all,
	     message(N, expand_role, @arg1?name, @arg1?value)).

expand_role(N, Role:name, Cache:int) :->
	"Expand a cache"::
	rdf_cache_cardinality(Cache, SetSize),
	(   SetSize < 15
	->  forall(rdf_cache_result(Cache, I, Value),
		   send(N, add_child_from_cache, Value, Role, Cache))
	;   rdf_cache_result(Cache, I, Value),
	    (	I == 11
	    ->	!,
		send(N, son, rdf_more_node(Role, Cache, 11))
	    ;	send(N, add_child_from_cache, Value, Role, Cache),
		fail
	    )
	;   true
	).

add_child(N, Resource:name, Role:name, Before:[node], Son:rdf_node) :<-
	"Create node for resource in role"::
	NewTerm =.. [Role, Resource],
	new(Son, NewTerm),
	send(N, son, Son, Before).

add_child(N, Resource:name, Role:name, Before:[node]) :->
	"Create node for resource in Role"::
	get(N, add_child, Resource, Role, Before, _Son).
	
add_child_from_cache(N, Resource:name, Role:name, Cache:int, Before:[node]) :->
	"Create node for resource in Role"::
	get(N, add_child, Resource, Role, Before, Son),
	send(Son, slot, cache, Cache).
	
show_more(N, MoreNode:rdf_more_node, Role:name, Count:int) :->
	"Show next Count nodes on Role"::
	get(N?caches, value, Role, Cache),
	get(MoreNode, here, Here),
	End is Here + Count,
	(   rdf_cache_result(Cache, I, Value),
	    I >= Here,
	    send(N, add_child_from_cache, Value, Role, Cache, MoreNode),
	    I >= End, !
	;   true
	),
	rdf_cache_cardinality(Cache, Cardinality),
	(   End < Cardinality
	->  send(MoreNode, here, End)
	;   send(MoreNode, destroy)
	).
	
:- pce_group(changes).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The module rdf_cache monitors changes in  the background and informs the
main loop to update visualisation due  to   a  changed cache result. The
task of this group of methods is to   update  the tree with as little as
possible effort.  There are still two pitfalls:

	* If the cache has grown too big it must add a `more' node
	* Elements added at the end a cache that is not the last will
	  be added as last child.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

update(N, Cache:[int]) :->
	(   get(N, hypered, editor, _)
	->  true
	;   get(N, collapsed, @on)
	->  send(N, update_can_expand)
	;   get(N, caches, Caches),
	    (   Cache == @default
	    ->	send(Caches, for_all,
		     message(N, update_role, @arg1?name, @arg1?value))
	    ;	get(Caches?members, find, @arg1?value == Cache, Att),
		get(Att, name, Role),
		send(N, update_role, Role, Cache)
	    ),
	    send(N, update_can_expand)
	).

update_role(N, Role:name, Cache:int) :->
	"Update results for a cache"::
	debug(update, '~p: Updating role ~w for cache ~w', [Role, Cache]),
	get(N, sons, Sons),
	get(Sons, find_all, @arg1?cache == Cache, Existing),
	(   rdf_cache_cardinality(Cache, 0)
	->  send(Existing, for_all,			% lost last one
		 message(@arg1, delete_tree))
	;   (   get(Existing, find,
		    message(@arg1, instance_of, rdf_more_node),
		    MoreNode)
	    ->  send(Existing, delete, MoreNode),
		get(MoreNode, here, Here),
		send(MoreNode, update)
	    ;   true
	    ),

	    (   rdf_cache_result(Cache, I, R),
		(   nonvar(Here), I > Here
		->  !
		;   get(Existing, head, Node),
		    get(Node, resource, R)
		->  send(Existing, delete_head),
		    fail			% OK: next one
		;   get(Existing, find, @arg1?resource == R, Node)
		->  delete_upto(Node, Existing),
		    fail
		;   insert_node(R, Role, Existing, Cache, N),
		    fail
		)
	    ;   send(Existing, for_all,
		     message(@arg1, delete_tree))
	    )
	),
	free(Existing).
	    
delete_upto(R, Existing) :-
	get(Existing, delete_head, Node),
	(   Node == R
	->  true
	;   send(Node, delete_tree),
	    delete_upto(R, Existing)
	).

insert_node(R, Role, Existing, Cache, Parent) :-
	get(Parent?sons, find,
	    and(@arg1?resource == R,		% same resource
		@arg1?class_name == Role,	% same role
	        @arg1?cache == @nil),		% not in a cache
	    Node), !,				% --> reuse
	(   get(Existing, head, Next)
	->  send(Node, move_before, Next)
	;   send(Node, move_after)
	),
	send(Node, slot, cache, Cache).
insert_node(R, Role, Existing, Cache, Parent) :-
	get(Existing, head, Next), !, 		% inserted
	get(Parent, add_child, R, Role, Next, NewNode),
	send(NewNode, slot, cache, Cache).
insert_node(R, Role, _, Cache, Parent) :-
	get(Parent?sons, find,
	    and(message(@arg1, instance_of, rdf_more_node),
		@arg1?role == Role),
	    MoreNode), !,			% insert before MoreNode
	get(Parent, add_child, R, Role, MoreNode, NewNode),
	send(NewNode, slot, cache, Cache).
insert_node(R, Role, _, Cache, Parent) :-
	get(Parent, add_child, R, Role, NewNode),
	send(NewNode, slot, cache, Cache).

:- pce_group(event).

:- pce_global(@rdf_node_recogniser, make_rdf_node_recogniser).

make_rdf_node_recogniser(G) :-
	new(P, popup_gesture(@receiver?popup)),
	new(C1, click_gesture(left, '', single,
			      message(@receiver,
				      on_left_click))),
			      
	new(C2, click_gesture(left, '', double,
			      message(@receiver,
				      on_double_left_click))),
	new(G, handler_group(P, C1, C2)).
			      

popup(N, Popup:popup) :<-
	call_rules(N, popup(N, Popup)).

event(N, Ev:event) :->
	"Handle node-event"::
	(   send_super(N, event, Ev)
	->  true
	;   send(Ev, post, N?image, @rdf_node_recogniser)
	).

on_left_click(N) :->
	"Select the current node"::
	send(N?tree, selected, N).

clicked(N, _:graphical) :->
	"Resource has been clicked inside me"::
	send(N, on_left_click).

on_double_left_click(N) :->
	"Select the current node"::
	get(N, class_name, Role),
	get(N?tree, selectable, Roles),
	(   (	Roles == @nil
	    ;	send(Roles, member, Role)
	    )
	->  send(N?tree, open_node, N)
	;   send(N, report, warning, 'Cannot open %s class', Role)
	).

:- pce_group(edit).

delete_resource(N) :->
	"Delete a class or individual"::
	get(N, resource, Resource),
	send(@display, confirm,
	     string('Delete resource %s?', Resource)),
	rdfe_delete(Resource).

delete_hyper(N, Hyper:hyper) :->
	(   get(Hyper, forward_name, editor)
	->  Update = true
	;   true
	),
	send_super(N, delete_hyper, Hyper),
	(   Update == true
	->  send(N, update)
	;   true
	).

:- pce_end_class(rdf_node).


:- pce_begin_class(rdf_class_node, rdf_node).


:- pce_group(edit).

new(N, Role:name) :->
	"Create a child in specified Role"::
	send_class(N, node, collapsed(@off)),
	get(N, resource, Resource),
	new(C, rdf_create_node(Resource, Role)),
	(   get(N?sons, find,
		and(message(@arg1, instance_of, rdf_more_node),
		    @arg1?role == Role),
		MoreNode)
	->  send(N, son, C, MoreNode)	% insert before the more-node
	;   send(N, son, C)
	),
	new(_, hyper(N, C, editor, node)),
	send(N?window, compute),
	send(C, get_focus).

new_class(N) :->
	"Create subclass of this class"::
	send(N, new, rdf_class_node).

new_individual(N) :->
	"Create indivisual of this class"::
	send(N, new, rdf_individual_node).

:- pce_end_class.


:- pce_begin_class(rdf_individual_node, rdf_node).
:- pce_end_class.


:- pce_begin_class(rdf_property_node, rdf_node).
:- pce_end_class.


:- pce_begin_class(owl_restriction_node, rdf_node).
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
variable(cache,    int*,  get, "Cache I belong to").

initialise(N, Role:name, Cache:int, Here:int) :->
	"Create `more' button"::
	rdf_cache_cardinality(Cache, Size),
	send_super(N, initialise, new(D, figure)),
	send(N, slot, role, Role),
	send(N, slot, size, Size),
	send(N, slot, here, Here),
	send(N, slot, cache, Cache),
	send(D, pen, 1),
	send(D, border, 2),
	send(D, format, new(Fmt, format(vertical, 1, @on))),
	send(Fmt, adjustment, vector(center)),
	send(N, update_more),
	send(N, collapsed, @nil).

update(N) :->
	"Update for changed cardinality of the cache"::
	get(N, cache, Cache),
	rdf_cache_cardinality(Cache, Size),
	(   get(N, size, Size)
	->  true
	;   send(N, slot, size, Size),
	    send(N, update_more)
	).

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


		 /*******************************
		 *	       EDIT		*
		 *******************************/

:- pce_begin_class(rdf_create_node, node,
		   "Create a new instance").

variable(cache,	int*, get, "Associated cache (left @nil)").

initialise(N, Parent:name, Role:name) :->
	send_super(N, initialise, new(D, rdf_create_dialog(Parent, Role))),
	send(D, pen, 1),
	send(N, collapsed, @nil).

resource_created(N, Resource:name, Role:name) :->
	get(N, parents, chain(Parent)),
	send(Parent, add_child, Resource, Role, N).

get_focus(N) :->
	send(N?image, advance).

delete_item(N, _D:graphical) :->
	send(N, destroy).

:- pce_end_class(rdf_create_node).

:- pce_begin_class(rdf_create_dialog, dialog,
		   "Create instance or class").

variable(role,     name, get, "Role of the new individual").
variable(resource, name, get, "Context Class").

initialise(D, Parent:name, Role:name) :->
	send(D, slot, role, Role),
	send(D, slot, resource, Parent),
	send_super(D, initialise, string('Create %s', Role?label_name)),
	rdf_global_id(NS:_, Parent),
	send(D, append, new(rdf_ns_menu(NS))),
	send(D, append, new(rdf_id_item), right),
	send(D, append, new(C, button(create, message(D, create_resource)))),
	send(D, append, button(done)),
	send(C, default_button, @on),
	send(D, '_compute_desired_size').

done(D) :->
	(   get(D, contained_in, Container),
	    send(Container, has_send_method, delete_item)
	->  send(Container, delete_item, D)
	;   send(D, destroy)
	).

create_resource(N) :->
	get(N, member, namespace, NSI),
	get(NSI, selection, NS),
	get(N, member, id, IDI),
	get(IDI, selection, Label),
	local_uri_from_label(NS, Label, Local),
	atom_concat(NS, Local, Resource),
	rdfe_transaction(send(N, do_create_resource, Resource, Label)),
	send(IDI, clear),
	(   get(N, contained_in, Container),
	    send(Container, has_send_method, resource_created)
	->  get(N, role, Role),
	    send(Container, resource_created, Resource, Role)
	;   true
	).

do_create_resource(N, Resource:name, Label:name) :->
	"Create a new resource"::
	get(N, resource, Super),
	(   get(N, role, rdf_class_node)
	->  rdfe_assert(Resource, rdf:type, rdfs:'Class'),
	    rdfe_assert(Resource, rdfs:subClassOf, Super)
	;   rdfe_assert(Resource, rdf:type, Super)
	),
	rdfe_assert(Resource, rdfs:label, literal(Label)).

local_uri_from_label(_, Label, Local) :-
	new(S, string('%s', Label)),
	send(S, translate, ' ', '_'),
	get(S, value, Local),
	free(S).

:- pce_end_class(rdf_create_dialog).


:- pce_begin_class(rdf_ns_menu, menu,
		   "Prompt for namespace").

initialise(M, Default:[name], Msg:[code]*) :->
	send_super(M, initialise, namespace, cycle, Msg),
	findall(NS, rdf_db:ns(NS, _), List0),
	sort(List0, List),
	(   member(NS, List),
	    rdf_db:ns(NS, Full),
	    send(M, append, menu_item(Full, @default, NS)),
	    fail
	;   true
	),
	(   Default \== @default,
	    rdf_db:ns(Default, FullDefault)
	->  send(M, selection, FullDefault)
	;   true
	),
	send(M, show_label, @off).

:- pce_end_class(rdf_ns_menu).

:- pce_begin_class(rdf_id_item, identifier_item,
		   "Enter a local id").

initialise(ID, Default:[name]) :->
	send_super(ID, initialise, id, Default),
	send(ID, show_label, @off).

typed(Id, Ev:event) :->
	(   get(Ev, id, 27)
	->  send(Id?device?node, destroy) % hack!
	;   send_super(Id, typed, Ev)
	).

:- pce_end_class(rdf_id_item).
