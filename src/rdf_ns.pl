/*  $Id$

    Developed in the MIA project
    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 2000 University of Amsterdam. All rights reserved.
*/

:- module(rdf_ns,
	  [
	  ]).
:- use_module(library(pce)).
:- use_module(library(persistent_frame)).
:- use_module(library(toolbar)).
:- use_module(library(pce_report)).
:- use_module(library(broadcast)).
:- use_module(library(lists)).
:- use_module(semweb(rdf_edit)).

:- pce_autoload(identifier_item, library(pce_identifier_item)).

:- pce_begin_class(rdf_namespace_window, persistent_frame,
		   "Query/Create namespaces").

initialise(F) :->
	send_super(F, initialise, 'RDF/XML namespaces'),
	send(F, append, new(TD, tool_dialog)),
	send(new(B, rdf_namespace_browser), below, TD),
	send(new(report_dialog), below, B),
	send(F, fill_dialog, TD).

fill_dialog(F, D:tool_dialog) :->
	send(D, append, new(File, popup(file))),
	send_list(File, append,
		  [ menu_item(new, message(F, new_namespace)),
		    gap,
		    menu_item(exit, message(F, destroy))
		  ]).

new_namespace(F) :->
	"Prompt for a new namespace"::
	new(D, dialog('Define new namespace')),
	send(new(report_dialog), below, D),
	send(D, append, new(IDItem, identifier_item(id))),
	send(D, append, new(URIItem, identifier_item(uri)), right),
	send(D, append, button(create, message(D, return, ok))),
	send(D, append, button(cancel, message(D, return, cancel))),
	send(IDItem, length, 6),
	send(URIItem, length, 50),
	send(URIItem, selection, 'http://'),
	send(D, default_button, create),
	send(D, transient_for, F),
	send(D, modal, transient),
	repeat,
	(   get(D, confirm_centered, F?area?center, Reply)
	->  (   Reply == ok
	    ->  get(IDItem, selection, ID),
		get(URIItem, selection, URI),
		catch(rdfe_transaction(rdfe_register_ns(ID, URI),
				       define_namespace),
		      E, true),
		(   var(E)
		->  !, send(D, destroy)
		;   message_to_string(E, Message),
		    send(D, report, error, Message),
		    fail
		)
	    ;   !, send(D, destroy)
	    )
	;   !
	).


:- pce_end_class(rdf_namespace_window).


:- pce_begin_class(rdf_namespace_browser, browser,
		   "Show defined namespaces").

class_variable(size, size, size(60, 10)).

initialise(B) :->
	send_super(B, initialise, 'RDF Namespaces'),
	send(B, tab_stops, vector(60)),
	send(B, select_message,
	     message(B, copy_ns, @arg1)),
	send(B, update),
	listen(B, rdf_ns(_),
	       send(B, update)).

unlink(B) :->
	unlisten(B),
	send_super(B, unlink).

copy_ns(_B, Di:dict_item) :->
	"Copy namespace expansion"::
	get(Di, object, NS),
	send(@display, copy, NS).

update(B) :->
	findall(ID, rdf_db:ns(ID, _), IDS0),
	sort(IDS0, IDS),
	send(B, clear),
	forall(member(ID, IDS), send(B, append, ID)).

append(B, Id:name) :->
	rdf_db:ns(Id, Name),
	send_super(B, append,
		   dict_item(Id, string('%s\t%s', Id, Name), Name)).

:- pce_end_class(rdf_namespace_browser).
