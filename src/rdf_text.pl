/*  File:    rdf_text.pl
    Author:  Jan Wielemaker
    Created: Jun 25 2003
    Purpose: Generic and specialized text objects
*/

:- module(rdf_text, []).
:- use_module(library(pce)).
:- use_module(library(debug)).
:- use_module(owl).
:- use_module(semweb(rdf_edit)).
:- use_module(semweb(rdfs)).
:- use_module(rdf_template).
:- use_module(particle).

:- pce_autoload(editable_text, library(pce_editable_text)).
:- pce_autoload(partof_hyper,  library(hyper)).

:- pce_begin_class(rdf_resource_text, text,
		   "Visualize a resource as a text object").
:- use_class_template(rdf_visual).
:- use_class_template(rdf_resource_template).

variable(resource,  name,  get, "Represented resource").
class_variable(colour, colour, blue).

initialise(T, Resource:name, _Container:[object]) :->
	"Create from resource and table"::
	send(T, slot, resource, Resource),
	get(T, label, Label),
	send_super(T, initialise, Label).

label(T, Label:char_array) :<-
	"Compute label from resource"::
	get(T, resource, Resource),
	rdfs_ns_label(Resource, Label).

resource(T, Resource:name) :->
	"Modify the represented resource"::
	(   get(T, resource, Resource)
	->  true
	;   send(T, slot, resource, Resource),
	    get(T, label, Label),
	    send(T, string, Label)
	).

:- pce_global(@rdf_resource_text_recogniser,
	      make_resource_text_recogniser).

make_resource_text_recogniser(G) :-
	new(CG, click_gesture(left, '', single,
			      message(@receiver, on_left_click))),
	new(PG, popup_gesture(@receiver?popup)),
	new(AE, handler(area_enter, message(@receiver, entered, @on))),
	new(AX, handler(area_exit, message(@receiver, entered, @off))),
	new(G, handler_group(CG, PG, AE, AX)).

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
	->  send(TF, report, status, TF?resource)
	;   send(TF, report, status, '')
	).
	

entered(TF, Enter:bool) :->
	(   Enter == @on,
	    send(TF, clipped_by_window)
	->  send(@unclip_window, attach, TF)
	;   true
	).

:- pce_end_class(rdf_resource_text).


		 /*******************************
		 *	   LITERAL TEXT		*
		 *******************************/

:- pce_begin_class(rdf_literal_text, editable_text,
		   "Text object for literal values").

variable(literal,   name*, get, "Represented object (=value)").

initialise(LT, Value:prolog) :->
	(   Value = literal(Text)
	->  true
	;   Text = Value
	),
	send_super(LT, initialise, Text),
	send(LT, margin, 400, wrap),
	send(LT, slot, literal, Text).

refresh(LT) :->
	"Update represented text"::
	get(LT, subject, Subject),
	get(LT, predicate, Predicate),
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

:- pce_global(@rdf_literal_text_popup,
	      make_literal_text_popup).
:- pce_global(@rdf_literal_text_recogniser,
	      new(popup_gesture(@receiver?popup))).

make_literal_text_popup(P) :-
	new(P, popup),
	send_list(P, append,
		  [ menu_item(delete,
			      message(@arg1, delete))
		  ]).


popup(_T, Popup:popup) :<-
	Popup = @rdf_literal_text_popup.

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
	;   get(T, contained_in, Dev),
	    send(Dev, has_send_method, rdf_modified)
	->  send(Dev, rdf_modified, T, literal(OldText), literal(NewText))
	;   debug(edit, '~p: Container cannot handle edit', [T])
	).

arm(T, Arm:bool) :->
	send(T, underline, Arm).

:- pce_end_class(rdf_literal_text).

