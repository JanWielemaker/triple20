/*  File:    rdf_text.pl
    Author:  Jan Wielemaker
    Created: Jun 25 2003
    Purpose: Generic and specialized text objects
*/

:- module(rdf_text, []).
:- use_module(library(pce)).
:- use_module(owl).
:- use_module(semweb(rdf_edit)).
:- use_module(semweb(rdfs)).
:- use_module(rdf_template).
:- use_module(particle).

:- pce_autoload(editable_text, library(pce_editable_text)).
:- pce_autoload(rdf_explorer,  rdf_explorer).
:- pce_autoload(partof_hyper,  library(hyper)).

:- pce_begin_class(rdf_resource_text, text,
		   "Visualize a resource as a text object").
:- use_class_template(rdf_visual).

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

:- pce_global(@rdf_resource_text_popup,
	      make_resource_text_popup).
:- pce_global(@rdf_resource_text_recogniser,
	      make_resource_text_recogniser).

make_resource_text_popup(P) :-
	new(P, popup),
	Text = @arg1,
	send_list(P, append,
		  [ menu_item(hierarchy_location,
			      message(Text, show_details, hierarchy),
			      condition := message(Text,
						   can_show_details,
						   hierarchy)),
		    menu_item(details,
			      message(Text, show_details, table),
			      condition := message(Text,
						   can_show_details,
						   table)),
		    menu_item(show_id,
			      message(Text, report, inform, Text?resource)),
		    menu_item(copy_id_to_clipboard,
			      message(Text, copy)),
		    menu_item(copy_as_xml_identifier,
			      message(Text, copy, xml_identifier)),
		    menu_item(copy_as_xml_attribute,
			      message(Text, copy, xml_attribute)),
		    menu_item(view_rdf_source,
			      message(Text, view_rdf_source)),
		    menu_item(diagram_,
			      message(Text, open_diagram))
		  ]).


make_resource_text_recogniser(G) :-
	new(CG, click_gesture(left, '', single,
			      message(@receiver, on_left_click))),
	new(PG, popup_gesture(@receiver?popup)),
	new(AE, handler(area_enter, message(@receiver, entered, @on))),
	new(AX, handler(area_exit, message(@receiver, entered, @off))),
	new(G, handler_group(CG, PG, AE, AX)).

popup(_T, Popup:popup) :<-
	Popup = @rdf_resource_text_popup.

event(T, Ev:event) :->
	(   send_super(T, event, Ev)
	->  true
	;   send(@rdf_resource_text_recogniser, event, Ev)
	).

entered(TF, Enter:bool) :->
	send(TF, underline, Enter),
	(   Enter == @on,
	    send(TF, clipped_by_window)
	->  send(@unclip_window, attach, TF)
	;   true
	).

:- pce_group(edit).

show_details(T, How:{hierarchy,table}) :->
	"Show details in format"::
	(   get(T, frame, Frame),
	    send(Frame, has_send_method, show_resource)
	->  get(T, resource, Resource),
	    send(Frame, show_resource, Resource, How)
	).

can_show_details(T, _How:{hierarchy,table}) :->
	"Test if we are embedded in a context that can show details"::
	get(T, frame, Frame),
	send(Frame, has_send_method, show_resource).


view_rdf_source(T) :->
	"Open Prolog editor on RDF source"::
	get(T, resource, Id),
	(   rdf_source_location(Id, File:Line)
	->  edit(file(File, line(Line)))
	;   send(T, report, warning, 'Cannot find source for %s', Id)
	).

copy(T, As:[{resource,xml_identifier,xml_attribute}]) :->
	"Copy resource to clipboard"::
	get(T, resource, Resource),
	(   As == xml_identifier
	->  rdf_global_id(NS:Local, Resource),
	    new(Copy, string('%s:%s', NS, Local))
	;   As == xml_attribute
	->  rdf_global_id(NS:Local, Resource),
	    new(Copy, string('&%s;%s', NS, Local))
	;   Copy = Resource
	),
	send(@display, copy, Copy).

:- pce_group(diagram).

open_diagram(T) :->
	"Open triple diagram from resource Id"::
	get(T, resource, Resource),
	get(T, rdf_diagram, Diagram),
	send(Diagram, resource, Resource),
	send(Diagram, expose).

rdf_diagram(T, Diagram:rdf_explorer) :<-
	"Get associated RDF explorer"::
	get(T, frame, Frame),
	(   get(Frame, hypered, rdf_explorer, Diagram)
	->  true
	;   new(Diagram, rdf_explorer),
	    new(_, partof_hyper(Frame, Diagram, rdf_explorer, hierarchy))
	).

:- pce_group(edit).

prompt(T, Subject:name, Property:name, Default:[name], Label:[name]) :->
	"Prompt for a (new) value"::
	get(T, device, AL),
	send(AL, prompt_value,
	     message(T, edited, @arg1, @arg2),
	     Subject,
	     Property,
	     Default,
	     Label,
	     T).

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
	;   get(T, device, Dev),
	    send(Dev, has_send_method, rdf_modified)
	->  send(Dev, rdf_modified, T, literal(OldText), literal(NewText))
	;   true
	).

:- pce_end_class(rdf_literal_text).

