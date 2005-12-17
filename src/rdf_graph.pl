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
:- use_module(library(lists)).
:- use_module(library(print_graphics)).
:- use_module(semweb(rdf_db)).
:- use_module(triple20(rdf_rules)).

resource(expand,   image, image('expand.xpm')).
resource(collapse, image, image('collapse.xpm')).


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
		  [ menu_item(print, message(D, print)),
		    gap,
		    menu_item(clear, message(D, clear)),
		    menu_item(exit, message(DF, destroy))
		  ]),

	send_list(View, append,
		  [ menu_item(layout, message(D, layout)),
		    menu_item(collapse_all, message(D, mode, label)),
		    menu_item(expand_all, message(D, mode, sheet))
		  ]).

resource(DF, Resource:name, Mode:[{sheet, label}],
	 AllowBag:[bool], Where:at=[point]) :->
	"Add resource to the diagram"::
	get(DF, member, rdf_graph, D),
	send(D, append, Resource, Mode, AllowBag, Where).

resource_bag(DF, Resources:chain) :->
	"Add collapsed compound resource bag"::
	get(DF, member, rdf_graph, D),
	send(D, resource_bag, Resources).

clear(DF) :->
	"Clear the diagram window"::
	get(DF, member, rdf_graph, D),
	send(D, clear).

layout(DF) :->
	"Re-run graph layout"::
	get(DF, member, rdf_graph, D),
	send(D, layout).

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
:- use_class_template(print_graphics).

variable(mode,          {sheet,label}, get, "Current mode for members").
variable(check_link_to, hash_table,    get, "Objects whose links to check").

initialise(D) :->
	send(D, slot, check_link_to, new(hash_table)),
	send_super(D, initialise),
	send(D, recogniser, @arm_recogniser).	% allow arming window for
						% drag-and-drop

:- pce_group(content).

append(D, Resource:resource=name,
          Mode:mode=[{sheet, label}], AllowBag:[bool], Where:at=[point]) :->
	"Display a resource in Mode at Where"::
	get(D, append, Resource, Mode, AllowBag, Where, _Obj).

append(D, Resource:resource=name,
       Mode:mode=[{sheet, label}], AllowBag:[bool], Where:at=[point], Obj) :<-
	"Display a resource in Mode at Where"::
	(   get(D, rdf_object, Resource, AllowBag, Obj)
	->  (   Mode \== @default
	    ->	send(Obj, mode, Mode)
	    ;	true
	    ),
	    (   Where \== @default
	    ->	send(Obj, move, Where)
	    ;	true
	    )
	;   call_rules(D, create_resource_sheet(Resource, Mode, Obj)),
	    send(D, display, Obj, Where),
	    (	Where == @default
	    ->	(   get(D?graphicals, size, 1) % just me
		->  send(Obj, set, 50, 50)
		;   true		% use graph layout
		)
	    ;	true
	    ),
	    (	Mode == @default
	    ->	get(D, mode, TheMode)
	    ;	TheMode = Mode
	    ),
	    (	TheMode == label
	    ->	true
	    ;	send(Obj, mode, sheet)
	    )
	),
	send(Obj, request_compute),
	send(Obj, link_to_me).


rdf_object(D, Resource:name, AllowBag:[bool],
	   RdfObject:'rdf_object|rdf_resource_bag') :<-
	"Find object vizualising me"::
	(   get(D, member, Resource, RdfObject)
	->  true
	;   AllowBag \== @off,
	    get(D?graphicals, find,
		and(message(@arg1, instance_of, rdf_resource_bag),
		    message(@arg1, contains, Resource)),
		RdfObject)
	).

resource_bag(D, Resources:chain, At:[point]) :->
	"Show bag representation"::
	chain_list(Resources, List),
	call_rules(D, create_resource_bag(List, BagGR)),
	send(D, display, BagGR, At).

:- pce_group(compute).

check_link_to(D, O:name) :->
	get(D, check_link_to, Table),
	send(Table, append, O, @on).

compute(D) :->
	get(D, check_link_to, Table),
	send(Table, for_all,
	     message(D, update_link_to, @arg1)),
	send(Table, clear),
	send_super(D, compute).


update_link_to(D, O:name) :->
	(   rdf(S, _P, O),
	    send(D, update_objects_showing, S, update),
	    fail
	;   true
	).

update_objects_showing(D, Resource:name, Message:name) :->
	"Send all objects representing Resource ->Message"::
	(    get(D, member, Resource, RdfObject)
	->   send(RdfObject, Message)
	;    true
	),
	send(D?graphicals, for_all,
	     if(and(message(@arg1, instance_of, rdf_resource_bag),
		    message(@arg1, contains, Resource)),
		message(@arg1, Message))).


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
	send(D, compute),		% create all links
	get(D, visible, Area),
	get(D?graphicals, find_all,
	    or(message(@arg1, instance_of, rdf_object),
	       message(@arg1, instance_of, rdf_resource_bag)),
	    Objects),
	send(Objects?head, layout,
	     nominal := 50,
	     area := Area,
	     iterations := 1000,
	     network := Objects).


mode(D, Mode:{label,sheet}) :->
	"Vizualisation mode of graphicals"::
	(   get(D, mode, Mode)
	->  true
	;   send(D, slot, mode, Mode),
	    send(D?graphicals, for_all,
	     if(message(@arg1, instance_of, rdf_object),
		message(@arg1, mode, Mode)))
	).


:- pce_group(event).

preview_drop(_D, What:any, _Where:point) :->
	send(What, has_get_method, resource).

drop(D, What:any, Where:point) :->
	"Move/add object"::
	(   get(What, attribute, slot_label_for, P),
	    get(What, device, Obj)
	->  send(Obj, expand_slot_values, P, Where)
	;   send(What, instance_of, rdf_icon_label),
	    get(What, device, Bag),
	    send(Bag, instance_of, rdf_resource_bag),
	    \+ send(Bag?area, in, Where)
	->  get(What, resource, R),
	    send(Bag, excommunicate, R, D, Where)
	;   send(What, has_get_method, resource),
	    get(What, resource, R),
	    send(D, append, R, at := Where)
	).

arm(_G, _Val:bool) :->
	"Preview activity"::
	true.

:- pce_group(popup).

resource(_, _) :<-
	fail.

print_graphics(P) :->
	"More unique name"::
	send(P, print).

popup(P, Popup:popup) :<-
	call_rules(P, popup(P, Popup)).

:- pce_global(@rdf_graph_recogniser,
	      make_rdf_graph_recogniser).

make_rdf_graph_recogniser(G) :-
	new(G, key_binding),
	send_list(G,
		  [ function('\\C-l', layout)
		  ]).

event(P, Ev:event) :->
	(   send_super(P, event, Ev)
	->  true
	;   send(@rdf_graph_recogniser, event, Ev)
	).

:- pce_end_class(rdf_graph).


		 /*******************************
		 *	      OBJECTS		*
		 *******************************/

:- pce_begin_class(rdf_object, rdf_composite_label,
		   "Represent an object").

variable(mode,          {sheet, label}, get, "Current display mode").
variable(layout_status, bool := @off,   get, "Status of auto-layout").
variable(outdated,      chain,          get, "Outdated features").

class_variable(mode, {sheet, label}, label).

:- send(@class, handle, handle(w/2, 0, link, north)).
:- send(@class, handle, handle(w/2, h, link, south)).
:- send(@class, handle, handle(0, h/2, link, west)).
:- send(@class, handle, handle(w, h/2, link, east)).

initialise(O, R:name, Mode:[{sheet, label}]) :->
	"Create from resource"::
	send(O, slot, outdated, new(chain)),
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
	send(O, request_compute, content).

update_content(O) :->
	send(O, clear, destroy),
	get(O, resource, Resource),
	get(O, mode, Mode),
	send(O, append, rdf_object_mode_button(Mode)),
	send(O, append_resource, Resource),
	(   get(O, mode, label)
	->  RelOnly = @on
	;   RelOnly = @off
	),
	get(O, sheet_attributes, Attrs),
	send(Attrs, for_all,
	     message(O, show_attribute, @arg1, RelOnly)).

sheet_attributes(O, Atts:chain) :<-
	"Return a chain holding the attributes for the sheet"::
	call_rules(O, diagram_object_slots(O, Slots)),
	chain_list(Atts, Slots).

show_attribute(V, P:name, RelOnly:[bool]) :->
	"Add given attribute to visualization"::
	get(V, resource, S),
	findall(O, rdf(S, P, O), Os),
	link_attributes(Os, P, V, Rest),
	(   (   Rest == []
	    ;   RelOnly == @on
	    )
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

%	link_attributes2(+Values, +Property, +RdfObj, -RestAttrs)
%	
%	Represent the Values on Property using links if the other object
%	is present. Return the remaining attributes   in RestAttrs to be
%	displayed on the rdf_object instance itself.

link_attributes2([], _, _, []).
link_attributes2([H|Tail], P, V, Rest) :-
	atom(H),
	get(V, device, Dev),
	get(Dev, rdf_object, H, RdfObject), !,
	canonical_property(P, V-RdfObject, CP, F-T),
	(   get(F, connections, T, CList),
	    get(CList, find,
		and(message(@arg1, instance_of, rdf_arc),
		    @arg1?resource == CP),
		_)
	->  true			% existing connection
	;   new(_, rdf_arc(F, CP, T))
	),
	link_attributes2(Tail, P, V, Rest).
link_attributes2([H|T], P, V, [H|Rest]) :-
	link_attributes2(T, P, V, Rest).

%	canonical_property(+P, +F-T, -CP, -CF-CT)
%	
%	We display relations between graphicals in the diagram using the
%	`canonical property' to avoid getting   two properties displayed
%	in the same location.  Now  it  is   a  bit  hard  to define the
%	canonical property. As a way to get   going  we say that for any
%	property that has an owl:inverseOf the relation it is an inverse
%	of is the canonical one.

canonical_property(P, F-T, CP, T-F) :-
	rdf_has(P, owl:inverseOf, CP), !,
	debug(rdf_object, '~p: Using inverse property ~p', [P, CP]).
canonical_property(P, FT, P, FT).


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
	send(Dev, has_send_method, check_link_to),
	get(V, resource, O),
	send(Dev, check_link_to, O).

show_my_subjects(V) :->
	"Show objects having me as subject"::
	get(V, device, Dev),
	get(V, resource, O),
	(   setof(S, not_show_subject(O,Dev,S), List),
	    length(List, Len)
	->  (   Len > 10
	    ->	get(V, ask_howmany, Len, Show)
	    ;	Show = Len
	    ),
	    get(V, bottom_side, Bottom),
	    get(V, left_side, Left),
	    X  is Left + 150,
	    Y0 is Bottom + 50,
	    State = state(Y0, 0),
	    (	member(S, List),
		arg(1, State, Y),
		send(Dev, append, S, label, at := point(X, Y)),
		Y2 is Y + 30,
		nb_setarg(1, State, Y2),
		arg(2, State, C0),
		C is C0+1,
		nb_setarg(2, State, C),
		C == Show
	    ->	true
	    ;	true
	    ),
	    send(V, update)
	;   rdf(_, _, O)
	->  send(V, report, status, 'All subjects already in graph')
	;   send(V, report, warning, 'No subjects link to me')
	).

%	not_show_subject(+Object, +Graph, -Subject)
%	
%	Find the subjects relating to me that are not (yet) displayed
%	in the graph.

not_show_subject(O, V, S) :-
	send(V, has_get_method, rdf_object), !,
	rdf(S, _, O),
	\+ get(V, rdf_object, S, _).
not_show_subject(O, _, S) :-
	rdf(S, _, O).

ask_howmany(V, Count:int, Show:int) :<-
	"Ask how many to display"::
	new(D, rdf_dialog(V, 'Found many')),
	sformat(Label, 'Found ~D subjects', Count),
	send(D, append, label(title, Label)),
	send(D, append, button(show_5, message(D, return, 5))),
	send(D, append, button(show_25, message(D, return, 25))),
	send(D, append, button(cancel, message(D, destroy))),
	get(D, confirm, Show0),
	send(D, destroy),
	Show = Show0.


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

request_compute(O, What:[name]) :->
	(   atom(What)
	->  get(O, outdated, OutDated),
	    (   send(OutDated, member, What)
	    ->  true
	    ;   send(OutDated, append, What),
		send_super(O, request_compute)
	    )
	;   send_super(O, request_compute)
	).

compute(O) :->
	get(O, outdated, Outdated),
	(   send(Outdated, delete, content),
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

close_related_nodes(O) :->
	"Close all nodes I'm related to"::
	get(O, connections, Cs),
	get(Cs, map, when(@arg1?from == O, @arg1?to, @arg1?from), Others),
	send(Others, for_all, message(@arg1, destroy)),
	send(O, update).


expand_slot_values(Obj, P:name, Where:point) :->
	"Expand all values for slot P"::
	get(Obj, device, Graph),
	get(Obj, resource, S),
	get(Where, clone, Pos),
	send(@display, busy_cursor),	% doesn't work?
	(   rdf(S,P,O),
	    atom(O),			% only resources
	    send(Graph, append, O, at := Pos),
	    send(Pos, plus, point(5, 20)),
	    fail
	;   send(@display, busy_cursor, @nil)
	).

:- pce_end_class(rdf_object).


:- pce_begin_class(rdf_object_mode_button, bitmap,
		   "Switch object modes").

initialise(B, Mode:{label,sheet}) :->
	(   Mode == label
	->  Resource = expand
	;   Resource = collapse
	),
	send_super(B, initialise, resource(Resource)).

:- pce_global(@rdf_object_mode_button_recogniser,
	      new(click_gesture(left, '', single, message(@receiver, execute)))).

event(B, Ev:event) :->
	(   send_super(B, event, Ev)
	->  true
	;   send(@rdf_object_mode_button_recogniser, event, Ev)
	).

execute(B) :->
	get(B, device, Obj),
	(   get(Obj, mode, label)
	->  send(Obj, mode, sheet),
	    send(Obj, expose)
	;   send(Obj, mode, label)
	).

:- pce_end_class(rdf_object_mode_button).


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

initialise(C, Subject:graphical, Predicate:name, Object:graphical) :->
	send(C, slot, resource, Predicate),
	send_super(C, initialise, Subject, Object, @rdf_arc_link),
	call_rules(C, label(Predicate, Label)),
	send(C, tag, Label).

:- pce_end_class(rdf_arc).


:- pce_begin_class(rdf_bag_member_arc, connection,
		   "Relate between bag and instance").

variable(resource, name, get, "Represented predicate resource").
class_variable(colour, colour, grey60).

:- pce_global(@rdf_bag_member_link, new(link(link, link))).

initialise(C, Subject:rdf_resource_bag, Object:rdf_object) :->
	send_super(C, initialise, Subject, Object, @rdf_bag_member_link),
	get(C, class_variable_value, colour, Colour),
	send(C, colour, Colour).

:- pce_end_class(rdf_bag_member_arc).


		 /*******************************
		 *	    COMPOSITES		*
		 *******************************/

:- pce_begin_class(rdf_resource_bag, figure,
		   "Display bag of resources").
:- use_class_template(rdf_container).

variable(resources,	 chain,	   get,  "Represented resources").
variable(by_exemplar,    sheet,    none, "Exemlars by class").
variable(show_exemplars, int,	   none, "Default # Exemplars").  
variable(outdated,	 chain,    get,  "Outdated features").

class_variable(show_exemplars, int, 3).

:- send(@class, handle, handle(w/2, 0, link, north)).
:- send(@class, handle, handle(w/2, h, link, south)).
:- send(@class, handle, handle(0, h/2, link, west)).
:- send(@class, handle, handle(w, h/2, link, east)).

initialise(RB, Resources:chain) :->
	send_super(RB, initialise),
	send(RB, slot, by_exemplar, new(sheet)),
	send(RB, slot, outdated, new(chain)),
	send(RB, pen, 1),
	send(RB, shadow, 2),
	send(RB, border, 5),
	send(RB, background, colour(white)),
	send(RB, slot, resources, Resources),
	send(RB, update_content).

contains(RB, Resource:name) :->
	"True if I represent Resource"::
	get(RB, resources, Chain),
	send(Chain, member, Resource).

delete(RB, Resource:name) :->
	"Delete a resource from the bag"::
	get(RB, resources, Chain),
	send(Chain, delete, Resource),
	send(RB, update_content).	% request_compute?

mode(_RB, _Mode:{label,sheet}) :->
	"Dummy for integration purposes"::
	true.

link_to_me(RB) :->
	"Make objects that refer to my resource use a link"::
	get(RB, device, Dev),
	send(Dev, has_send_method, check_link_to),
	get(RB, resources, Resources),
	send(Resources, for_all,
	     message(Dev, check_link_to, @arg1)).


		 /*******************************
		 *	      CONTENT		*
		 *******************************/

show_exemplars(RB, Class:name, Show:int) :<-
	(   get(RB, slot, by_exemplar, Sheet),
	    get(Sheet, value, Class, Show)
	->  true
	;   get(RB, slot, show_exemplars, Show)
	).

show_exemplars(RB, Class:[name], Show:int) :->
	(   Class == @default
	->  send(RB, slot, show_exemplars, Show)
	;   get(RB, slot, by_exemplar, Sheet),
	    send(Sheet, value, Class, Show)
	),
	send(RB, update_content).	% TBD: compute

more(RB, Class:name, More:[int]) :->
	default(More, 10, Extra),
	get(RB, show_exemplars, Class, N0),
	N is N0+Extra,
	send(RB, show_exemplars, Class, N).

update_content(RB) :->
	"Create the content"::
	send(RB, clear, destroy),
	get_chain(RB, resources, Resources),
	key_type(Resources, Pairs),
	keysort(Pairs, TypeResources),
	join(TypeResources, Bags),
	(   member(bag(C, Rs, Count), Bags),
	    call_rules(RB, label(C, CLabel)),
	    send(RB, nl),
	    send(RB, append, CLabel),
	    send(RB, append, text(string(' (%d)', Count))),
	    get(RB, show_exemplars, C, ExN),
	    (	nth1(I, Rs, R),
		call_rules(RB, label(R, RLabel)),
		send(RB, nl, 20),
		send(RB, append, RLabel),
		I =:= ExN
	    ->  send(RB, nl, 20),
		send(RB, append, rdf_bag_more(C, Count, ExN))
	    ;   true
	    ),
	    fail
	;   true
	),
	send(RB, request_compute, layout).

join([], []).
join([C-R|T0], [bag(C,[R|Rs],N)|T]) :-
	same_type(C, T0, Rs, T1),
	length(Rs, N0),
	N is N0 + 1,
	join(T1, T).

same_type(C, [C-R|T0], [R|Rs], T) :- !,
	same_type(C, T0, Rs, T).
same_type(_, T, [], T).

key_type([], []).
key_type([R|T0], [C-R|T]) :-
	type_of(R, C),
	key_type(T0, T).

type_of(R, C) :-
	rdf_has(R, rdf:type, C), !.
type_of(_, C) :-
	rdf_equal(C, rdfs:'Resource').

	
:- pce_group(layout).

nl(O, Indent:[int]) :->
	send(O, display, new(I, layout_graphical(nl))),
	(   Indent \== @default
	->  send(I, width, Indent)
	;   true
	).

append(O, Gr:graphical) :->
	send(O, display, Gr),
	send(O, request_compute, layout).


		 /*******************************
		 *	       UPDATE		*
		 *******************************/

update(RB) :->
	send(RB, request_compute, links).

request_compute(RB, What:[name]) :->
	(   atom(What)
	->  get(RB, outdated, OutDated),
	    (   send(OutDated, member, What)
	    ->  true
	    ;   send(OutDated, append, What),
		send_super(RB, request_compute)
	    )
	;   send_super(RB, request_compute)
	).

compute(RB) :->
	get(RB, outdated, Outdated),
	(   send(Outdated, delete, links)
	->  send(RB, update_links)
	;   true
	),
	(   send(Outdated, delete, layout)
	->  send(RB, update_layout)
	;   true
	),
	send_super(RB, compute).

update_links(RB) :->
	"Create outgoing links to other objects"::
	get(RB, device, Dev),
	(   send(Dev, has_get_method, rdf_object)
	->  get_chain(RB, resources, Resources),
	    (	member(S, Resources),
		rdf(S, P, O),
		atom(O),
		get(Dev, rdf_object, O, RdfObject),
		\+ send(RB, connected_with_predicate, P, RdfObject),
		new(_, rdf_arc(RB, P, RdfObject)),
		fail
	    ;   true
	    )
	;   true
	).

connected_with_predicate(RB, P:name, RdfObject:graphical) :->
	"Test whether RB is connected to RdfObject using P"::
	get(RB, connections, RdfObject, CList),
	get(CList, find,
	    and(message(@arg1, instance_of, rdf_arc),
		@arg1?resource == P),
	    _).

update_layout(O) :->
	"Update the layout"::
	get_chain(O, graphicals, Grs),
	update_layout(Grs, 0, 0, 0, O).


		 /*******************************
		 *	ADD/DELETE OBJECTS	*
		 *******************************/

excommunicate(RB, R:name, Dev:device, Where:point) :->
	"Show object on the workspace"::
	get(Dev, append, R, @default, @off, Where, Obj),
	new(_, rdf_bag_member_arc(RB, Obj)).


		 /*******************************
		 *	       EVENTS		*
		 *******************************/

:- pce_global(@rdf_resource_bag_recogniser,
	      make_rdf_resource_bag_recogniser).

make_rdf_resource_bag_recogniser(G) :-
	new(G, move_gesture(left)).

event(O, Ev:event) :->
	(   get(Ev, id, ms_left_down),
	    debug(rdf_object, 'MS-left-down on ~p~n', [O]),
	    \+ ( get(O, find, Ev,
		     or(message(@arg1, instance_of, rdf_icon_label),
			message(@arg1, instance_of, rdf_bag_more),
			message(@arg1, instance_of, bitmap)), % Hack
		     Gr),
		 debug(rdf_object, 'Found ~p~n', [Gr])),
	    send(@rdf_resource_bag_recogniser, event, Ev)
	->  true
	;   send_super(O, event, Ev)
	).

:- pce_end_class(rdf_resource_bag).


:- pce_begin_class(rdf_bag_more, device,
		   "Indicate there are more").

variable(for_class, name, get, "Class to show more exemplars").

initialise(B, Class:name, Count:int, Show:int) :->
	send(B, slot, for_class, Class),
	send_super(B, initialise),
	send(B, format, format(vertical, 1, @off)),
	More is min(10, Count-Show),
	(   More > 0
	->  send(B, display, text('...', left, bold)),
	    send(B, display,
		 new(M10, text(string('+%d', More), left, bold))),
	    send(M10, attribute, message,
		 message(@arg1, more, @arg2, More))
	;   true
	),
	(   Show \== 3
	->  send(B, display, new(E03, text('=3', left, bold))),
	    send(E03, attribute, message,
		 message(@arg1, show_exemplars, @arg2, 3))
	;   true
	),
	send(B?graphicals, for_all,
	     if(message(@arg1, has_get_method, message),
		and(message(@arg1, underline, @on),
		    message(@arg1, colour, blue)))).

clicked(B) :->
	get(B, find, @event,
	    message(@arg1, has_get_method, message),
	    Text),
	get(Text, message, Msg),
	send(Msg, forward, B?device, B?for_class).

:- pce_global(@rdf_bag_more_recogniser,
	      new(click_gesture(left, '', single,
				message(@receiver, clicked)))).

event(B, Ev:event) :->
	(   send_super(B, event, Ev)
	->  true
	;   send(@rdf_bag_more_recogniser, event, Ev)
	).

:- pce_end_class(rdf_bag_more).



		 /*******************************
		 *	       LAYOUT		*
		 *******************************/

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


		 /*******************************
		 *	       RULES		*
		 *******************************/

:- begin_rules(rdf_graph_frame, rdf_graph_frame).

%	menu_item(-Group, -Item)
%	
%	Determine the popup item for resources displayed in the graph
%	view.

menu_item(graph, close).
menu_item(graph, close_other_nodes).
menu_item(graph, close_related_nodes).
menu_item(graph, show_my_subjects).
menu_item(Group, Item) :-
	super::menu_item(Group0, Item),
	(   Group0 == select
	->  Group = view
	;   Group = Group0
	).

%	diagram_object_slots(+Object, -Slots)
%	
%	Determine the slots displayed by Object.

diagram_object_slots(Object, Slots) :-
	get(Object, resource, Resource),
	inner::diagram_resource_slots(Resource, Slots).

diagram_resource_slots(Resource, Slots) :-
	findall(P, rdf(Resource, P, _), Ps),
	sort(Ps, Slots).

%	create_resource_sheet(+Resource, +Mode, -Graphical)
%	
%	Create a graphical to represent the resource Resource in the
%	diagram view.   Mode is one of 'sheet' or 'label'.

create_resource_sheet(Resource, Mode, Sheet) :-
	new(Sheet, rdf_object(Resource, Mode)).

%	create_resource_bag(+Resources, -Bag)
%	
%	Create a graphical Bag that represents the set of resources
%	Resources.

create_resource_bag(Resources, Bag) :-
	new(Bag, rdf_resource_bag(Resources)).

:- end_rules.

