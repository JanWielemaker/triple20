/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(rdf_graph, []).
:- use_module(library(pce)).
:- use_module(library(persistent_frame)).
:- use_module(library(toolbar)).
:- use_module(library(pce_tagged_connection)).
:- use_module(library(debug)).
:- use_module(semweb(rdf_db)).
:- use_module(triple20(rdf_rules)).

resource(expand,   image, image('expand.xpm')).
resource(collapse, image, image('collapse.xpm')).


user:t :-
	D = @g,
	free(D),
	send(new(D, rdf_graph_frame), open),
	send(D, resource, 'file:tigertail/fischer_maid_buying_fish.jpg').


:- pce_begin_class(rdf_graph_frame, persistent_frame).

variable(history, history, get, "Modification history").

initialise(DF) :->
	send_super(DF, initialise, 'RDF relation explorer'),
	send(DF, slot, history, new(H, history(message(DF, restore, @arg1)))),
	send(DF, append, new(TD, tool_dialog)),
	send(new(D, rdf_graph), below, TD),
	send(new(report_dialog), below, D),
	send(DF, fill_dialog, TD),
	send(TD, append, ?(H, button, backward)),
	send(TD, append, ?(H, button, forward)).

fill_dialog(DF, TD) :->
	"Fill the tool dialog"::
	get(DF, member, rdf_graph, D),
	send(TD, append, new(File, popup(file))),
	send(TD, append, new(View, popup(view))),

	send_list(File, append,
		  [ menu_item(exit, message(DF, destroy))
		  ]),

	send_list(View, append,
		  [ menu_item(layout, message(D, layout)),
		    menu_item(collapse_all, message(D, collapse_all)),
		    menu_item(expand_all, message(D, expand_all)),
		    menu_item(clear, message(D, clear))
		  ]).

resource(DF, Resource:name, Mode:[{sheet, label}]) :->
	get(DF, member, rdf_graph, D),
	send(D, append, Resource, Mode).

:- pce_group(triple20).

open_resource(DF, R:name, How:name) :->
	"Open a resource in the given way"::
	get(DF?display?frames, find,
	    message(@arg1, instance_of, rdfs_explorer),
	    Triple20),
	send(Triple20, open_resource, R, How).

:- pce_end_class(rdf_graph_frame).


		 /*******************************
		 *	  DIAGRAM WINDOW	*
		 *******************************/

:- pce_begin_class(rdf_graph, picture,
		   "Display an RDF diagram").
:- use_class_template(rdf_arm).

initialise(D) :->
	send_super(D, initialise),
	send(D, recogniser, @arm_recogniser).	% allow arming window for
						% drag-and-drop

:- pce_group(content).

append(D, Resource:name, Mode:[{sheet, label}], Where:[point]) :->
	(   get(D, rdf_object, Resource, Obj)
	->  (   Mode \== @default
	    ->	send(Obj, mode, Mode)
	    ;	true
	    ),
	    (   Where \== @default
	    ->	send(Obj, move, Where)
	    ;	true
	    )
	;   send(D, display, new(Obj, rdf_object(Resource, Mode)), Where),
	    (	Where == @default
	    ->	(   get(D?graphicals, size, 1) % just me
		->  send(Obj, set, 50, 50)
		;   true		% use graph layout
		)
	    ;	true
	    ),
	    (	Mode == label
	    ->	true
	    ;	send(Obj, mode, sheet)
	    )
	),
	send(Obj, request_compute),
	send(Obj, link_to_me).

rdf_object(D, Resource:name, RdfObject:rdf_object) :<-
	"Find object vizualising me"::
	get(D, member, Resource, RdfObject).

:- pce_group(state).

state(D, State:prolog) :<-
	"Describe the state of the graph as a Prolog term"::
	get_chain(D, graphicals, Grs),
	sub_states(Grs, State).

sub_states([], []).
sub_states([H0|T0], [H|T]) :-
	send(H0, has_get_method, state),
	get(H0, state, H), !,
	sub_states(T0, T).
sub_states([_|T0], T) :-
	sub_states(T0, T).

state(D, State:prolog) :->
	send(D, clear),
	restore_state(State, D).

restore_state([], _).
restore_state([H|T], D) :-
	add_state(H, D),
	restore_state(T, D).

add_state(Term, D) :-
	create(Term, Obj),
	send(D, display, Obj).

create(Term+Msg, Obj) :- !,		% NOTE: a+b+c = +(+(a, b), c)
	create(Term, Obj),
	send(Obj, Msg).
create(Term, Obj) :-
	new(Obj, Term).
	
:- pce_group(actions).

layout(D) :->
	get(D?graphicals, find_all,
	    message(@arg1, instance_of, rdf_object),
	    Objects),
	send(Objects?head, layout, network := Objects).

collapse_all(D) :->
	send(D?graphicals, for_all,
	     if(message(@arg1, instance_of, rdf_object),
		message(@arg1, mode, label))).


expand_all(D) :->
	send(D?graphicals, for_all,
	     if(message(@arg1, instance_of, rdf_object),
		message(@arg1, mode, sheet))).



:- pce_group(event).

preview_drop(_D, What:any, _Where:point) :->
	send(What, has_get_method, resource).

drop(D, What:any, Where:point) :->
	"Move/add object"::
	(   get(What, attribute, slot_label_for, P),
	    get(What, device, Obj)
	->  send(Obj, expand_slot_values, P, Where)
	;   send(What, has_get_method, resource),
	    get(What, resource, R),
	    send(D, append, R, @default, Where)
	).

arm(_G, _Val:bool) :->
	"Preview activity"::
	true.

:- pce_end_class(rdf_graph).


		 /*******************************
		 *	      OBJECTS		*
		 *******************************/

:- pce_begin_class(rdf_object, rdf_composite_label,
		   "Represent an object").

variable(mode,          {sheet, label}, get, "Current display mode").
variable(layout_status, bool := @off,   get, "Status of auto-layout").
variable(req_update_content, bool := @off,   get, "Require update content").
variable(update_links,  bool := @off,   get, "Update link status?").

class_variable(mode, {sheet, label}, label).

:- send(@class, handle, handle(w/2, 0, link, north)).
:- send(@class, handle, handle(w/2, h, link, south)).
:- send(@class, handle, handle(0, h/2, link, west)).
:- send(@class, handle, handle(w, h/2, link, east)).

initialise(O, R:name, Mode:[{sheet, label}]) :->
	"Create from resource"::
	send_super(O, initialise, R),
	send(O, name, R),		% allow for <-member
	send(O, format, @nil),
	(   Mode \== @default
	->  send(O, slot, mode, Mode)
	;   true
	),
	send(O, opaque, @off),
	send(O, pen, 1),
	send(O, shadow, 2),
	send(O, border, 5),
	send(O, background, colour(white)).


mode(O, Mode:{sheet, label}) :->
	"Switch between a label and sheet"::
	(   get(O, mode, Mode)
	->  true
	;   send(O, slot, mode, Mode),
	    send(O, update)
	).


device(O, Dev:device*) :->
	"Update attributes for sheet-mode"::
	send_super(O, device, Dev),
	(   Dev \== @nil
	->  send(O, update)
	;   true
	).


update(O) :->
	(   get(O, req_update_content, @off)
	->  send(O, slot, req_update_content, @on),
	    send(O, request_compute)
	;   true
	).

update_content(O) :->
	send(O, slot, req_update_content, @off),
	send(O, clear, destroy),
	get(O, resource, Resource),
	(   get(O, mode, label)
	->  send(O, append, new(I, bitmap(resource(expand)))),
	    send(I, recogniser, click_gesture(left, '', single,
					      message(O, mode, sheet))),
	    send(O, append_resource, Resource)
	;   send(O, append, new(I, bitmap(resource(collapse)))),
	    send(I, recogniser, click_gesture(left, '', single,
					      message(O, mode, label))),
	    send(O, append_resource, Resource),
	    get(O, sheet_attributes, Attrs),
	    send(Attrs, for_all,
		 message(O, show_attribute, @arg1))
	).

sheet_attributes(O, Atts:chain) :<-
	"Return a chain holding the attributes for the sheet"::
	get(O, resource, S),
	findall(P, rdf(S, P, _), Ps),
	sort(Ps, Unique),
	chain_list(Atts, Unique).

show_attribute(V, P:name) :->
	"Add given attribute to visualization"::
	get(V, resource, S),
	findall(O, rdf(S, P, O), Os),
	link_attributes(Os, P, V, Rest),
	(   Rest == []
	->  true
	;   send(V, nl, 30),
	    get(V, append_resource, P, Gr),
	    send(Gr, attribute, slot_label_for, P),
	    send(V, print, ' = '),
	    append_values(Rest, V, P)
	).

link_attributes(Values, P, V, NoLinkedValues) :-
	get(V, device, Dev),
	send(Dev, has_get_method, rdf_object), !,
	link_attributes2(Values, P, V, NoLinkedValues).
link_attributes(Values, _, _, Values).

link_attributes2([], _, _, []).
link_attributes2([H|T], P, V, Rest) :-
	atom(H),
	get(V, device, Dev),
	get(Dev, rdf_object, H, RdfObject), !,
	new(_, rdf_arc(V, P, RdfObject)),
	link_attributes2(T, P, V, Rest).
link_attributes2([H|T], P, V, [H|Rest]) :-
	link_attributes2(T, P, V, Rest).

append_values([], _, _).
append_values([H], V, P) :- !,
	get(V, append_resource, H, Gr),
	send(Gr, attribute, value_for, P).
append_values([H|T], V, P) :-
	get(V, append_resource, H, Gr),
	send(Gr, attribute, value_for, P),
	send(V, print, ', '),
	append_values(T, V, P).

link_to_me(V) :->
	"Make objects that refer to my resource use a link"::
	get(V, device, Dev),
	send(Dev, has_get_method, rdf_object),
	get(V, resource, O),
	(   rdf(S, _P, O),
	    get(Dev, rdf_object, S, SObj),
	    send(SObj, update), % TBD: check if attribute is active
	    fail
	;   true
	).

:- pce_group(state).

state(V, State:prolog) :<-
	"Describe state"::
	State = rdf_object(R, Mode)+set(X,Y),
	get(V, resource, R),
	get(V, mode, Mode),
	get(V, position, point(X,Y)).


:- pce_group(event).

:- pce_global(@rdf_object_recogniser,
	      make_rdf_object_recogniser).

make_rdf_object_recogniser(G) :-
	new(G, move_gesture(left)).

event(O, Ev:event) :->
	(   get(Ev, id, ms_left_down),
	    \+ ( get(O, find, Ev,
		     or(message(@arg1, has_get_method, value_for),
			message(@arg1, has_get_method, slot_label_for),
			message(@arg1, instance_of, bitmap)), % Hack
		     Gr),
		 debug(rdf_object, 'Found ~p~n', [Gr])),
	    send(@rdf_object_recogniser, event, Ev)
	->  true
	;   send_super(O, event, Ev)
	).

:- pce_group(layout).

nl(O, Indent:[int]) :->
	send(O, display, new(I, layout_graphical(nl))),
	(   Indent \== @default
	->  send(I, width, Indent)
	;   true
	).

compute(O) :->
	(   get(O, req_update_content, @on),
	    get(O, device, Dev),
	    Dev \== @nil
	->  send(O, update_content)
	;   true
	),
	(   get(O, layout_status, @off)
	->  send(O, slot, layout_status, @on),
	    send(O, format, @nil),	% TBD: cleaner solution
	    call_cleanup(send(O, update_layout),
			 send(O, slot, layout_status, @off))
	;   true
	),
	send_super(O, compute).

update_layout(O) :->
	"Update the layout"::
	get_chain(O, graphicals, Grs),
	update_layout(Grs, 0, 0, 0, O).

update_layout([], _, _, _, _).
update_layout([H|T], X, Y, HL, D) :-
	send(H, instance_of, layout_graphical), !,
	get(H, name, Command),
	(   Command == nl
	->  Y2 is Y+HL,
	    get(H, width, Indent),
	    update_layout(T, Indent, Y2, 0, D)
	;   update_layout(T, X, Y, HL, D)
	).
update_layout([H|T], X, Y, HL, D) :-
	send(H, set, X, Y),
	get(H, width, W),
	get(H, height, H1),
	HL2 is max(HL, H1),
	X2 is X + W,
	update_layout(T, X2, Y, HL2, D).


arm(TF, Val:bool) :->
	"Preview activity"::
	(   Val == @on
	->  send(TF, report, status, TF?resource),
	    send(TF, shadow, 4)
	;   send(TF, shadow, 2),
	    send(TF, report, status, '')
	).

		 /*******************************
		 *	   MENU ACTIONS		*
		 *******************************/

close(O) :->
	"Delete from graph and stick values back as slots"::
	(   get(O, connections, All),
	    get(All, map,
		when(@arg1?from == O, @arg1?to, @arg1?from),
		Connected)
	->  send(O, destroy),
	    send(Connected, for_all, message(@arg1, update))
	;   send(O, destroy)
	).

close_other_nodes(O) :->
	"Close all nodes but me"::
	get(O?device?graphicals, find_all,
	    and(message(@arg1, instance_of, rdf_object),
		@arg1 \== O),
	    Others),
	send(Others, for_all, message(@arg1, destroy)),
	send(O, update).

expand_slot_values(Obj, P:name, Where:point) :->
	"Expand all values for slot P"::
	get(Obj, device, Graph),
	get(Obj, resource, S),
	get(Where, clone, Pos),
	send(@display, busy_cursor),	% doesn't work?
	(   rdf(S,P,O),
	    send(Graph, append, O, @default, Pos),
	    send(Pos, plus, point(5, 20)),
	    fail
	;   send(@display, busy_cursor, @nil)
	).

:- pce_end_class(rdf_object).


:- pce_begin_class(layout_graphical, graphical).

initialise(LG, Name:name) :->
	send_super(LG, initialise, 0,0,0,0),
	send(LG, name, Name).

:- pce_end_class.



:- pce_begin_class(rdf_arc, tagged_connection,
		   "Relation in an RDF diagram").

variable(resource, name, get, "Represented predicate resource").

:- pce_global(@rdf_arc_link, new(link(link, link,
				      line(arrows := second)))).

initialise(C, Subject:rdf_object, Predicate:name, Object:rdf_object) :->
	send(C, slot, resource, Predicate),
	send_super(C, initialise, Subject, Object, @rdf_arc_link),
	call_rules(C, label(Predicate, Label)),
	send(C, tag, Label).

:- pce_end_class(rdf_arc).


		 /*******************************
		 *	       RULES		*
		 *******************************/

:- begin_rules(rdf_object, rdf_object).

menu_item(graph, close).
menu_item(graph, close_other_nodes).
menu_item(graph, values_to_nodes).
menu_item(Group, Item) :-
	super::menu_item(Group0, Item),
	(   Group0 == select
	->  Group = view
	;   Group = Group0
	).

:- end_rules.
