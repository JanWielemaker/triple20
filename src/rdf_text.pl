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


:- module(rdf_text, []).
:- use_module(library(pce)).
:- use_module(library(debug)).
:- use_module(library(pce_util)).
:- use_module(library(dragdrop)).
:- use_module(owl).
:- use_module(semweb(rdf_edit)).
:- use_module(semweb(rdfs)).
:- use_module(rdf_template).
:- use_module(rdf_rules).
:- use_module(library(sgml_write)).
:- use_module(library(memfile)).

:- pce_autoload(editable_text, library(pce_editable_text)).
:- pce_autoload(partof_hyper,  library(hyper)).

:- pce_begin_class(rdf_resource_text, text,
		   "Visualize a resource as a text object").
:- use_class_template(rdf_resource_template).

variable(resource,  name,  get, "Represented resource").
class_variable(colour, colour, blue).

initialise(T, Resource:name, _Container:[object]) :->
	"Create from resource and table"::
	send(T, slot, resource, Resource),
	get(T, label, Label),
	send_super(T, initialise, Label),
	send(@resource_texts, append, Resource, T).

unlink(T) :->
	get(T, resource, Resource),
	send(@resource_texts, delete, Resource, T),
	send_super(T, unlink).

update(T) :->
	"Update for possible databae changes"::
	get(T, label, Label),
	send(T, string, Label).

label(T, Label:char_array) :<-
	"Compute label from resource"::
	get(T, resource, Resource),
	(   call_rules(T, label_text(Resource, Label))
	->  true
	;   Label = Resource
	).

resource(T, Resource:name) :->
	"Modify the represented resource"::
	get(T, resource, Old),
	(   get(T, resource, Old)
	->  true
	;   send(@resource_texts, delete, Old, T),
	    send(T, slot, resource, Resource),
	    send(@resource_texts, append, Resource, T),
	    send(T, update)
	).

:- pce_global(@rdf_resource_text_recogniser,
	      make_resource_text_recogniser).

make_resource_text_recogniser(G) :-
	new(CG, click_gesture(left, '', single,
			      message(@receiver, on_left_click))),
	new(DG1, rdf_drop_gesture(left)),
	new(PG, popup_gesture(@receiver?popup)),
	new(DG2, rdf_drop_gesture(right)),
	new(G, handler_group(@arm_recogniser, CG, DG1, PG, DG2)).

popup(T, Popup:popup) :<-
	call_rules(T, popup(T, Popup)).

event(T, Ev:event) :->
	(   send_super(T, event, Ev)
	->  true
	;   send(@rdf_resource_text_recogniser, event, Ev)
	).

arm(TF, Arm:bool) :->
	send(TF, underline, Arm),
	(   Arm == @on
	->  send(TF, report, status, TF?resource),
	    (	send(TF, clipped_by_window),
		send(@grabbed_windows, empty)
	    ->  send(@unclip_window, attach, TF)
	    ;	true
	    )
	;   send(TF, report, status, '')
	).

:- pce_end_class(rdf_resource_text).

:- pce_begin_class(rdf_resource_id_text, rdf_resource_text).
:- pce_end_class.


:- pce_begin_class(rdf_drop_gesture, drag_and_drop_gesture).

initialise(DD, Button:name) :->
	send_super(DD, initialise, Button),
	send(DD, cursor, @default).

initiate(DD, Ev:event) :->
	send_super(DD, initiate, Ev),
	send(Ev?window, grab_pointer, @on).

terminate(DD, Ev:event) :->
	send_super(DD, terminate, Ev),
	send(Ev?window, grab_pointer, @off).

drag(DD, Ev:event) :->
	(   send(DD, activate)
	->  get(Ev, receiver, Dropping),
	    get(DD, source, Source),
	    (   get(Ev, inside_sub_window, Frame),
	        get(Ev, inside_sub_window, Frame, Window)
	    ->  debug(arm, 'Drag in ~p', [Window]),
	        (   send(Window, has_get_method, arm)
		->  get(Ev, window, Owner),
		    (	unfocussed(Owner,
			   get(Window, arm,
			       and(message(@receiver, has_send_method,
					   preview_drop),
				   message(@receiver, preview_drop,
					   Dropping)),
			       Target))
		    ->  send(DD, target, Source, Ev, Target)
		    ;	send(DD, target, Source, @nil, @nil)
		    )
		;   send_super(DD, drag, Ev)
		)
	    ;	send(DD, target, Source, @nil, @nil)
	    )
	;   true
	).


%	unfocussed(+Window, :Goal)
%
%	Removes the Window<-focus, executes Goal and restores the focus.
%	This is needed to avoid the `arm' event following the path of
%	the focus. An alternative (better) way might be to add an option
%	to event->post to ignore event-focus settings or allow for an
%	attribute to the event that will cause it to bypass focussing.

unfocussed(W, G) :-
	get(W, focus, F),
	get(W, focus_recogniser, R),
	get(W, focus_cursor, C),
	get(W, focus_button, B),
	(   F == @nil,
	    R == @nil
	->  G
	;   send(C, lock_object, @on),
	    send(W, focus, @nil, @nil),
	    call_cleanup(G,
			 (   send(W, focus, F, R, C, B),
			     send(C, lock_object, @off)))
	).

:- pce_end_class(rdf_drop_gesture).


		 /*******************************
		 *	      UPDATE		*
		 *******************************/

%	@resource_texts
%	
%	This object defines a table Resource --> resource text objects.
%	
%	NOTE: it is a bit more general and can be used by any simple
%	visualiser that may wish to change its visualisation if some
%	property of the principle resource is changed.  The required
%	methods are:
%	
%		register: send(@resource_texts, append, Resource, Obj)
%		un-	: send(@resource_texts, delete, Resource, Obj)
%	
%	The Obj must implement ->update.

:- pce_global(@resource_texts, new(chain_table)).

:- listen(rdf_transaction(X), text_changes(X)),
   listen(rdf_undo(X), text_changes(X)).

text_changes(X) :-
	debug(update, 'Transaction: ~w', [X]),
	rdfe_transaction_member(X, Action),
	action_resource(Action, Resource),
	get(@resource_texts, member, Resource, TextChain),
	debug(update, 'Updating resource-texts for ~p~n', [Resource]),
	send(TextChain, for_all, message(@arg1, update)),
	fail.
text_changes(_).

action_resource(assert(R, _, _), R).
action_resource(retract(R, _, _), R).
action_resource(update(R, _, _, _), R).


		 /*******************************
		 *	   LITERAL TEXT		*
		 *******************************/

:- pce_begin_class(rdf_literal_text, editable_text,
		   "Text object for literal values").

variable(subject,   name*, get, "Resource I belong to").
variable(predicate, name*, get, "Resource I belong to").
variable(literal,   name*, get, "Represented object (=value)").
variable(lang,      name*, get, "Language of the literal").
variable(type,      name*, get, "Type of the literal").

initialise(LT, Value:prolog, Subject:[name]*, Predicate:[name]*) :->
	get_text(Value, TypeOrLang, Text),
	send_super(LT, initialise, Text),
	send(LT, margin, 400, wrap),
	default(Subject, @nil, S),
	default(Predicate, @nil, P),
	send(LT, slot, subject, S),
	send(LT, slot, predicate, P),
	send(LT, slot, literal, Text),
	set_type_or_lang(TypeOrLang, LT).

%	get_text(+Object, -TypeOrLanguage, -Text)
%	
%	Extract Text and Language from an RDF Object.

get_text(literal(X), Lang, Text) :- !,
	get_text(X, Lang, Text).
get_text(lang(Lang, Text), lang(Lang), Text) :- !.
get_text(type(Type, Text), type(Type), Text) :- !.
get_text(Text, -, Text) :-
	atomic(Text).
get_text(XML, -, Text) :-
	is_xml(XML),
	new_memory_file(MF),
	open_memory_file(MF, write, Out),
	xml_write(Out, XML, []),
	close(Out),
	memory_file_to_atom(MF, Text),
	free_memory_file(MF).

is_xml(0) :- !, fail.			% move to SGML library?
is_xml([]).
is_xml([H|T]) :-
	is_xml(H), 
	is_xml(T).
is_xml(element(_, _, _)).

set_type_or_lang(-, LT) :-
	send(LT, slot, lang, @nil),
	send(LT, slot, type, @nil).
set_type_or_lang(lang(Lang), LT) :-
	send(LT, slot, lang, Lang),
	send(LT, slot, type, @nil).
set_type_or_lang(type(Type), LT) :-
	send(LT, slot, lang, @nil),
	send(LT, slot, type, Type).

update(LT) :->
	"Update represented text"::
	get(LT, subject, Subject), Subject \== @nil,
	get(LT, predicate, Predicate), Predicate \== @nil,
	(   rdf(Subject, Predicate, literal(Value))
	->  get_text(Value, TypeOrLang, Text),
	    send(LT, string, Text),
	    set_type_or_lang(TypeOrLang, LT)
	;   send(LT, literal, @nil),
	    send(LT, string, ''),
	    set_type_or_lang(-, LT)
	).

triple(T, Value:prolog) :<-
	"Find part the triple I belong to"::
	get(T, contained_in, C0),
	container_with_get_method(C0, triple_from_part, Container),
	get(Container, triple_from_part, T, Value).

subject(T, Subject:name) :<-
	get(T, triple, rdf(Subject, _, _)).

predicate(T, Predicate:name) :<-
	get(T, triple, rdf(_, Predicate, _)).

resource(LT, Object:prolog) :<-
	get(LT, object, Object).

object(LT, Object:prolog) :<-
	"Get RDF object: literal(Text)"::
	get(LT, literal, Text),
	Object = literal(Value),
	(   get(LT, lang, Lang), Lang \== @nil
	->  Value = lang(Lang, Text)
	;   get(LT, type, Type), Type \== @nil
	->  Value = type(Type, Text)
	;   Value = Text
	).

obtain_focus(T) :->
	"Start editing"::
	(   get(T, show_caret, @on)
	->  send_super(T, obtain_focus)
	;   send_super(T, obtain_focus),
	    send(T, save_parameter, background),
	    send(T, border, 0),
	    send(T, pen, 0),
	    send(T, background, colour(white))
	).

:- pce_global(@rdf_literal_text_recogniser,
	      new(handler_group(@arm_recogniser,
				popup_gesture(@receiver?popup)))).


popup(T, Popup:popup) :<-
	call_rules(T, popup(T, Popup)).

event(T, Ev:event) :->
	(   send_super(T, event, Ev)
	;   send(@rdf_literal_text_recogniser, event, Ev)
	).

delete(T) :->
	"Delete associated triple"::
	get(T, triple, rdf(Subject, Predicate, Object)),
	rdfe_transaction(rdfe_retractall(Subject, Predicate, Object),
			 delete_property).

forward(T) :->
	"Set new value"::
	get(T?string, value, NewText),
	send(T, modify, literal, NewText).


modify(T, What:{literal,lang,type}, Value:name) :->
	"Modify a value and forward the changes"::
	get(T, What, OldValue),
	(   Value == OldValue
	->  true
	;   get(T, subject, Subject),
	    get(T, predicate, Predicate),
	    Subject \== @nil,
	    Predicate \== @nil
	->  get(T, object, OldObject),
	    send(T, slot, What, Value),
	    get(T, object, NewObject),
	    catch(rdfe_transaction(rdfe_update(Subject, Predicate, OldObject,
					       object(NewObject)),
				   modify(What)),
		  E,
		  (   report_exception(T, E),
		      send(T, slot, What, OldValue),
		      fail
		  ))
	;   debug(edit, '~p: Container cannot handle edit', [T])
	).


report_exception(Gr, E) :-
	message_to_string(E, Message),
	send(Gr, report, error, Message).

set_language(T) :->
	(   get(T, lang, Lang), Lang \== @nil
	->  true
	;   Lang = @default
	),
	new(D, rdf_dialog(T, 'Select language')),
	send(D, append, new(I, rdf_language_item(language, Lang,
						 message(D, return, @arg1)))),
	send(D, standard_buttons,
	     button(ok, message(I, execute))),
	get(D, confirm, NewLang),
	send(D, destroy),
	send(T, modify, lang, NewLang).


arm(T, Arm:bool) :->
	send(T, underline, Arm),
	(   Arm == @on
	->  (   get(T, lang, Lang), Lang \== @nil
	    ->	(   iso_639(Lang, Language)
		->  true
		;   Language = unknown
		),
	        send(T, report, status, 'xml:lang="%s" <%s>', Lang, Language)
	    ;	get(T, type, Type), Type \== @nil
	    ->	rdfs_ns_label(Type, Label),
	        send(T, report, status, 'rdf:dataType="%s"', Label)
	    ;	send(T, report, status, '')
	    )
	;   send(T, report, status, '')
	).

copy_text(T) :->
	"Copy text"::
	get(T, literal, Text),
	send(@display, copy, Text).

:- pce_group(drag_and_drop).

preview_drop(T, Visual:visual*) :->
	(   Visual == @nil
	->  send(T, report, status, '')
	;   send(Visual, has_get_method, resource),
	    call_rules(T, drop_command(T, Visual, Cmd)),
	    get(Visual, resource, Resource),
	    rdfs_ns_label(Resource, RL),
	    send(T, report, status,
		 'Drop %s onto literal: %s', RL, Cmd?label_name)
	).

drop(T, Visual:visual) :->
	call_rules(T, drop(T, Visual)).

:- pce_end_class(rdf_literal_text).

