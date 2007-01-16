/*  $Id$

    Developed in the MIA project
    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 2000 University of Amsterdam. All rights reserved.
*/

:- module(rdf_literal_item, []).
:- use_module(library(pce)).
:- use_module('semweb/rdf_db').
:- use_module('semweb/rdfs').

:- pce_begin_class(rdf_literal_item, dialog_group,
		   "Enter literal value").

initialise(I,
	   Prop:property=name,
	   _Class:class=[name],		% use to find visualization
	   Default:[prolog],
	   Width:width=[int],
	   Height:height=[int]) :->
	rdfs_label(Prop, Label),
	default(Width, 40, W),
	default(Height, 1, H),
	send_super(I, initialise(Label)),
	(   H == 1
	->  send(I, append, new(TI, text_item(text_item))),
	    send(TI, length, W),
	    send(TI, show_label, @off),
	    send(TI, hor_stretch, 100)
	;   send(I, append, new(E, editor(@default, W, H))),
	    send(E, wrap, word),
	    send(E, attribute, hor_stretch, 100)
	),
	(   Default = literal(Selection)
	->  send(I, selection, Selection)
	;   true
	).

selection(I, Text:name) :<-
	"Return selection as a name"::
	(   get(I, member, editor, E)
	->  get(E, contents, String)
	;   get(I, member, text_item, TI),
	    get(TI, selection, String)
	),
	get(String, strip, canonise, S2),
	get(S2, value, Text),
	Text \== ''.

selection(I, Sel:prolog) :->
	"Set selection of the editor"::
	(   Sel = literal(Text)
	->  true
	;   Text = Sel
	),
	(   get(I, member, editor, E)
	->  send(E, contents, Text)
	;   get(I, member, text_item, TI),
	    send(TI, selection, Text)
	).
	
clear(I) :->
	"Clear the item"::
	send(I, selection, '').

message(OI, Msg:[code]*) :->		% added BJW
	get(OI, member, text_item, TI),
	send(TI, message, Msg).
message(OI, Msg:[code]*) :<-		% added BJW
	get(OI, member, text_item, TI),
	get(TI, message, Msg).

apply(OI, Always:[bool]) :->		% added BJW
	"Execute <-message"::
	(   (   Always == @on
	    ;   get(OI, modified, @on)
	    )
	->  (   get(OI, selection, Value),
	        get(OI, message, Message),
		send(Message, instance_of, code)
	    ->	send(Message, forward_receiver, OI, Value)
	    ;	true
	    )
	).



:- pce_group(buttons).

append_button(I, Name:name) :->
	new(B, button(Name)),
	send(B, label, image(resource(Name))),
	send(B, reference, point(0, B?height)),
	send(I, append, B, right).

append_delete_button(I) :->
	"Add a button to allow deleting this field"::
	send(I, append_button, cut).

cut(I) :->
	"Remove me from by device"::
	(   send(I?device, has_send_method, cut)
	->  send(I?device, cut, I)
	;   send(I, destroy)		% Not really cute
	).

:- pce_end_class(rdf_literal_item).

