/*  File:    tree.pl
    Author:  Jan Wielemaker
    Created: Jun  3 2003
    Purpose: Visualise RDF hierarchy
*/

:- module(rdf_tree_file, []).
:- use_module(library(pce)).
:- use_module(library(pce_unclip)).
:- use_module(semweb(rdf_db)).
:- use_module(semweb(rdfs)).
:- use_module(semweb(rdf_edit)).
:- use_module(particle).
:- use_module(rdf_template).
:- use_module(rdf_cache).
:- use_module(library(debug)).
:- use_module(library(hyper)).
:- use_module(library(broadcast)).

:- pce_autoload(rdf_create_dialog, rdf_create).


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
	send(KB, function, key_top_5, update),
	send(KB, function, '\\ef', find).

initialise(H, Root:[name]) :->
	send_super(H, initialise),
	(   Root == @default
	->  rdf_equal(rdfs:'Resource', TheRoot)
	;   TheRoot = Root
	),
	send(H, direction, list),
	send(H, level_gap, 15),
	send(H, neighbour_gap, 1),
	new(RootNode, rdf_root_node(TheRoot)),
	send(H, root, RootNode),
	listen(H, rdf_reset, send(H, clear)),
	listen(H, rdf_journal(_), send(H, update)),
	listen(H, rdf_transaction(TID), send(H, update_transaction, TID)).

unlink(H) :->
	unlisten(H),
	send_super(H, unlink).

clear(H) :->
	(   get(H, root, Root)
	->  send(Root?sons, for_all, message(@arg1, delete_tree)),
	    send(Root, update)
	;   true
	).

expand_root(H) :->
	"Expand the root node"::
	get(H, root, Root),
	send(Root, collapsed, @off).

:- pce_group(build).

create_node(_OT, Resource:name, Role:name, Node:node) :<-
	"Create a new node (using tree for proper context)"::
	NewTerm =.. [Role, Resource],
	new(Node, NewTerm).

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
	    once(path(Resource, Root, OT, Path))
	->  display_path(Path, OT, Node)
	;   send(OT, report, warning, 'Cannot find path to %s', Resource),
	    fail
	).
	
%	path(+Resource, +Root, +Tree, -Path)
%	
%	Find path from Resource to Root using the rules of Tree. Path is
%	a list of Resource-Role, where role is the visualiser role
%	(=class) to be used for the node.

path(Resource, Root, Tree, Path) :-
	path(Resource, Root, Tree, [Resource], Path).

path(Resource, Resource, _, _, [Resource-[]]) :- !.
path(Resource, Root, Tree, Visited, [Resource-Role|T]) :-
	call_rules(Tree, parent(Resource, Parent, Role)),
	\+ memberchk(Parent, Visited), !,
	debug(path, 'Trying parent ~p, role ~w', [Parent, Role]),
	path(Parent, Root, Tree, [Resource|Visited], T).

display_path([H-Role|_], OT, Node) :-
	get(OT, member, H, Node),
	(   Role == []
	;   get(Node, class_name, Role)
	), !.
display_path([H-Role|T], OT, Node) :-
	display_path(T, OT, Parent),
	get(Parent, add_child, H, Role, Node),
	send_class(Parent, node, collapsed(@off)).

%	->show_all_parents: Resource
%	
%	Show Resource as a child of  all   its  parents in the preferred
%	role. This is very tricky. We only use non-determinism selecting
%	the immediate parent as the  visualisation   of  all paths often
%	explodes. We also only  provide  all   solutions  of  the  first
%	parent/3 returned role (realised using the bagof call).

show_all_parents(OT, Resource:name) :->
	"Show all paths from the root to Resource"::
	get(OT?root, resource, Root),
	(   bagof(P, call_rules(OT, parent(Resource, P, Role)), Ps)
	->  (   member(Parent, Ps),
	        (   path(Parent, Root, OT, Path)
		->  display_path(Path, OT, ParentNode),
		    (	get(ParentNode?sons, find,
			    and(@arg1?class_name == Role,
				@arg1?resource == Resource),
			    Node)
		    ->	true
		    ;	get(ParentNode, add_child, Resource, Role, Node)
		    ),
		    send_class(ParentNode, node, collapsed(@off)),
		    send(Node, selected, @on)
		;   true
		),
		fail
	    ;   true
	    )
	;   true
	).

:- pce_group(event).

event(OT, Ev:event) :->
	"Deal with events"::
	(   send_super(OT, event, Ev)
	;   send(@onto_tree_recogniser, event, Ev)
	).

on_left_click(OT) :->
	"Deselect all nodes"::
	send(OT, selection, @nil).

selected(OT, Node:'node|graphical') :->
	"User selected a node"::
	(   send(Node, instance_of, node)
	->  send(OT, selection, Node?image)
	;   send(OT, selection, @nil),
	    send(Node?device, selection, Node)
	),
	(   get(OT, message, M),
	    M \== @nil,
	    send(Node, has_get_method, resource)
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

triple_from_part(_OT, From:graphical, Triple:prolog) :<-
	"Find triple represented by a node"::
	get(From, node, Node),
	get(Node?parents, head, Parent),
	get(Parent, resource, O),
	get(Node, resource, S),
	Triple = rdf(S, _, O).

:- pce_group(update).

update_transaction(T, TID:int) :->
	"Update after a transaction"::
	rdfe_transaction_member(TID, file(_)),
	send(T, update_label).

update_label(T) :->
	"Check all nodes for updated label classes"::
	send(T?root, for_all, message(@arg1, update_label)).

update(T) :->
	send(T?root, for_all, message(@arg1, update_label)),
	send(T?root, for_all, message(@arg1, update)).

:- pce_group(search).

find(OT, String:for=name, How:how=[name],
         Fields:predicates=[chain], Max:max=[int]) :->
	"Find from a string"::
	ReportTo = OT,
	statistics(cputime, CPU0),
	default(How, substring, TheHow),
	default(Max, 100, MaxCount),
	send(OT, selection, @nil),
	get(OT, domain, Domain),
	(   Fields == @default
	->  PlFields = [rdfs:label]
	;   chain_list(Fields, PlFields)
	),
	new(Hits, hash_table),
	(   rdfs_find(String, Domain, PlFields, TheHow, Subject),
	    \+ get(Hits, member, Subject),
	    send(Hits, append, Subject),
	    send(OT, show_hit, Subject),
	    get(Hits, size, Count),
	    (   Count > MaxCount
	    ->  true
	    ;   send(ReportTo, report, progress, 'Found %d ...', Count),
	        fail
	    )
	;   true
	),
	get(Hits, size, Count),
	(   Count == 0
	->  send(ReportTo, report, warning, 'No hits'),
	    send(OT, expand_domain)
	;   Count =< MaxCount
	->  statistics(cputime, CPU1),
	    CPU is CPU1 - CPU0,
	    send(ReportTo, report, done, 'completed in %.2f seconds', CPU)
	;   send(ReportTo, report, status, 'Shown first %d hits', MaxCount)
	).

show_hit(OT, Id:name) :->
	"Show hit of search"::
	get(OT, add, Id, Node),
	send(Node, selected, @on).

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


update_label(N) :->
	"Check for possibly changed label classification"::
	get(N, resource, Resource),
	call_rules(N, label_class(Resource, LabelClass)),
	(   get(N?image, class_name, LabelClass)
	->  true
	;   get(N, label, NewLabel),
	    send(N, image, NewLabel)
	).


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
	get(N?tree, create_node, Resource, Role, Son),
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
	debug(update, '~p: Updating role ~w for cache ~w', [N, Role, Cache]),
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
	head_of_next_role(Parent, Cache, Before), !,
	get(Parent, add_child, R, Role, Before, NewNode),
	send(NewNode, slot, cache, Cache).
insert_node(R, Role, _, Cache, Parent) :-
	get(Parent, add_child, R, Role, NewNode),
	send(NewNode, slot, cache, Cache).

%	head_of_next_role(+Node, +Cache, -Head)
%
%	Find the first node produced by the `next' cache.

head_of_next_role(N, Cache, Head) :-
	new(Ch, chain),
	send(N?caches, for_all, message(Ch, append, @arg1?value)),
	chain_list(Ch, Caches),
	append(_, [Cache|After], Caches),
	member(C2, After),
	get(N?sons, find, @arg1?cache == C2, Head), !.

:- pce_group(event).

:- pce_global(@rdf_node_recogniser, make_rdf_node_recogniser).

make_rdf_node_recogniser(G) :-
	new(P, popup_gesture(@receiver?popup)),
			      
	new(C2, click_gesture(left, '', double,
			      message(@receiver,
				      on_double_left_click))),
	new(G, handler_group(P, C2)).
			      

popup(N, Popup:popup) :<-
	call_rules(N, popup(N, Popup)).

event(N, Ev:event) :->
	"Handle node-event"::
	(   send_super(N, event, Ev)
	->  true
	;   send(Ev, post, N?image, @rdf_node_recogniser)
	).

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

:- pce_group(view).


show_all_parents(N) :->
	"Show all locations for this resource"::
	get(N, resource, Resource),
	get(N, tree, Tree),
	send(Tree, show_all_parents, Resource).


:- pce_group(debug).

print_cache_status(N) :->
	get(N, cache, MyCache),
	format('~p: from parent cache: ~w~n', [N, MyCache]),
	send(N?caches, for_all,
	     message(N, print_cache, @arg1?name)).

print_cache(N, Role:name) :->
	get(N?caches, value, Role, Cache),
	(   rdf_cache:cache_attributes(Cache, _Generation, Size)
	->  format('   ~w: ~w: size=~D~n', [Role, Cache, Size])
	;   rdf_cache:cache_empty(Cache, _Generation, Empty)
	->  format('   ~w: ~w: empty=~w~n', [Role, Cache, Empty])
	;   format('   ~w: ~w: <unknown>~n', [Role, Cache])
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
	send(N?window, normalise, C, y),
	send(C, show_dialog).

unrelate_resource(N) :->
	"Unlink from parent"::
	get(N, resource, Subject),
	get(N?parents?head, resource, Object),
	rdf_has(Subject, rdfs:subClassOf, Object, P),
	rdfe_transaction(rdfe_retractall(Subject, P, Object)).

new_class(N) :->
	"Create subclass of this class"::
	send(N, new, rdf_class_node).

new_individual(N) :->
	"Create indivisual of this class"::
	send(N, new, rdf_individual_node).

:- pce_end_class.


:- pce_begin_class(rdf_individual_node, rdf_node).

unrelate_resource(N) :->
	"Unlink from parent"::
	get(N, resource, Subject),
	get(N?parents?head, resource, Object),
	rdf_has(Subject, rdf:type, Object, P),
	rdfe_transaction(rdfe_retractall(Subject, P, Object)).

:- pce_end_class.


:- pce_begin_class(rdf_property_node, rdf_node).
:- pce_end_class.


:- pce_begin_class(owl_restriction_node, rdf_node).
:- pce_end_class.

:- pce_begin_class(rdf_root_node, rdf_node).
:- pce_end_class.

:- pce_begin_class(rdf_orphan_node, rdf_node).
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
	send_super(N, initialise, new(more_figure)),
	send(N, slot, role, Role),
	send(N, slot, size, Size),
	send(N, slot, here, Here),
	send(N, slot, cache, Cache),
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

:- pce_begin_class(more_figure, figure,
		   "Showing more ... buttons").

initialise(F) :->
	send_super(F, initialise),
	send(F, pen, 1),
	send(F, border, 2),
	send(F, format, new(Fmt, format(vertical, 1, @on))),
	send(Fmt, adjustment, vector(center)).

arm(TF, Val:bool) :->
	"Unclip if obscured by window"::
	(   Val == @on
	->  (	send(TF, clipped_by_window),
		send(@grabbed_windows, empty)
	    ->  debug(arm, 'Arming ~p', [TF]),
		send(TF, pen, 0),
	        send(@unclip_window, attach, TF),
		send(TF, pen, 1)
	    ;	true
	    )
	;   true
	).

event(TF, Ev:event) :->
	(   send(@arm_recogniser, event, Ev)
	->  true
	;   send_super(TF, event, Ev)
	).

:- pce_end_class.



		 /*******************************
		 *	       EDIT		*
		 *******************************/

:- pce_begin_class(rdf_create_node, node,
		   "Create a new instance").

variable(cache,	int*, get, "Associated cache (left @nil)").

initialise(N, Parent:name, Role:name) :->
	new(D, rdf_create_dialog(Parent, Role, N)),
	send_super(N, initialise, new(_B, rdf_window_ghost(100, 50))),
	new(_, mutual_dependency_hyper(N, D, window, node)),
	send(N, collapsed, @nil).

resource_created(N, Resource:name, Role:name) :->
	get(N, parents, chain(Parent)),
	send(Parent, add_child, Resource, Role, N).

show_dialog(N) :->
	get(N, image, Ghost),
	send(Ghost, open).

delete_item(N, _D:graphical) :->
	send(N, destroy).

:- pce_end_class(rdf_create_node).

:- pce_begin_class(rdf_window_ghost, box,
		   "Maintain location of a dialog box").

client(B, W:window) :<-
	get(B, node, Node),
	get(Node, hypered, window, W).

geometry(B, X:[int], Y:[int], W:[int], H:[int]) :->
	send_super(B, geometry, X, Y, W, H),
	get(B, client, Client),
	get(B, display_position, point(DX, DY)),
					% -1: hack
	send(Client?frame, geometry, string('+%d+%d', DX-1, DY-1)).

open(B) :->
	get(B, client, W),
	get(B, frame, Frame),
	get(B, display_position, DP),
	send(W, transient_for, Frame),
	send(W, modal, transient),
	send(W, open, DP).

:- pce_end_class.


