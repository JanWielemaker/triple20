/*  File:    rdf_text.pl
    Author:  Jan Wielemaker
    Created: Jun 25 2003
    Purpose: Generic and specialized text objects
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
:- use_module(particle).

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

:- pce_begin_class(rdf_drop_gesture, drag_and_drop_gesture).

initialise(DD, Button:name) :->
	send_super(DD, initialise, Button, get_source := @arg1?resource),
	send(DD, cursor, @default).

initiate(DD, Ev:event) :->
	send_super(DD, initiate, Ev),
	send(Ev?window, grab_pointer, @on).


cancel(DD, Ev:event) :->
	send(Ev?window, grab_pointer, @off),
	send_super(DD, terminate, Ev).


terminate(DD, Ev:event) :->
	send(Ev?window, grab_pointer, @off),
	send_super(DD, terminate, Ev).


drag(DD, Ev:event) :->
	(   send(DD, activate)
	->  get(DD, source, Source),
	    (   get(Ev, inside_sub_window, Frame),
	        get(Ev, inside_sub_window, Frame, Window)
	    ->  debug(arm, 'Drag in ~p', [Window]),
	        (   send(Window, has_get_method, arm)
		->  (   unfocussed(Window, get(Window, arm, drop, Target))
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

:- pce_global(@resource_texts, new(chain_table)).

:- listen(rdf_transaction(X), text_changes(X)),
   listen(rdf_undo(X), text_changes(X)).

text_changes(X) :-
	debug(update, 'Transaction: ~w', [X]),
	rdfe_transaction_member(X, Action),
	arg(1, Action, Resource),
	get(@resource_texts, member, Resource, TextChain),
	debug(update, 'Updating resource-texts for ~p~n', [Resource]),
	send(TextChain, for_all, message(@arg1, update)),
	fail.
text_changes(_).


		 /*******************************
		 *	   LITERAL TEXT		*
		 *******************************/

:- pce_begin_class(rdf_literal_text, editable_text,
		   "Text object for literal values").

variable(subject,   name*, get, "Resource I belong to").
variable(predicate, name*, get, "Resource I belong to").
variable(literal,   name*, get, "Represented object (=value)").

initialise(LT, Value:prolog, Subject:[name]*, Predicate:[name]*) :->
	(   Value = literal(Text)
	->  true
	;   Text = Value
	),
	send_super(LT, initialise, Text),
	send(LT, margin, 400, wrap),
	default(Subject, @nil, S),
	default(Predicate, @nil, P),
	send(LT, slot, subject, S),
	send(LT, slot, predicate, P),
	send(LT, slot, literal, Text).

update(LT) :->
	"Update represented text"::
	get(LT, subject, Subject), Subject \== @nil,
	get(LT, predicate, Predicate), Predicate \== @nil,
	(   rdf(Subject, Predicate, literal(Text))
	->  send(LT, literal, Text),
	    send(LT, string, Text)
	;   send(LT, literal, @nil),
	    send(LT, string, '')
	).

object(LT, Object:prolog) :<-
	"Get RDF object: literal(Text)"::
	get(LT, literal, Text),
	Object = literal(Text).
	

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
	call_rules(T, popup(Popup)).

event(T, Ev:event) :->
	(   send_super(T, event, Ev)
	;   send(@rdf_literal_text_recogniser, event, Ev)
	).

delete(T) :->
	"Delete associated triple"::
	get(T, subject, Subject),
	get(T, predicate, Predicate),
	get(T, object, Text),
	rdfe_transaction(rdfe_retractall(Subject, Predicate,
					 literal(Text))).

forward(T) :->
	"Set new value"::
	get(T?string, value, NewText),
	get(T, literal, OldText),
	(   OldText == NewText
	->  true
	;   get(T, subject, Subject),
	    get(T, predicate, Predicate),
	    Subject \== @nil,
	    Predicate \== @nil
	->  rdfe_transaction(rdfe_update(Subject, Predicate,
					 literal(OldText),
					 object(literal(NewText))))
	;   get(T, contained_in, Dev),
	    send(Dev, has_send_method, rdf_modified)
	->  send(Dev, rdf_modified, T, literal(OldText), literal(NewText))
	;   debug(edit, '~p: Container cannot handle edit', [T])
	).

arm(T, Arm:bool) :->
	send(T, underline, Arm).

:- pce_end_class(rdf_literal_text).

