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
:- use_module(rdf_util).

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
	send(B, style, fallback, style(font := bold)),
	send(B, update),
	send(B, popup, new(P, popup)),
	Item = ?(B, dict_item, @event),
	HasItem = (condition := Item),
	send_list(P, append,
		  [ menu_item(set_fallback,
			      message(B, set_fallback, Item?key),
			      HasItem)
		  ]),
	listen(B, rdf_ns(_),
	       send(B, update)),
	listen(B, rdf_default_ns(File, _NS),
	       (   var(File),
		   send(B, update_default_ns))).

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
	(   rdf_default_ns([], Id)
	->  Style = fallback,
	    sformat(Label, '~w\t~w (fallback)', [Id, Name])
	;   Style = @default,
	    sformat(Label, '~w\t~w', [Id, Name])
	),
	send_super(B, append, dict_item(Id, Label, Name, Style)).

update_default_ns(B) :->
	"Update indication of default namespace"::
	get(B, dict, Dict),
	send(Dict?members, for_all, message(B, update_default_ns1, @arg1)).

update_default_ns1(_B, DI:dict_item) :->
	get(DI, key, Id),
	get(DI, object, Name),
	(   rdf_default_ns([], Id)
	->  send(DI, style, fallback),
	    sformat(Label, '~w\t~w (fallback)', [Id, Name]),
	    send(DI, label, Label)
	;   send(DI, style, @default),
	    sformat(Label, '~w\t~w', [Id, Name]),
	    send(DI, label, Label)
	).

:- pce_group(edit).

set_fallback(_B, NS:name) :->
	"Make this the fallback default namespace"::
	rdf_set_default_ns(_, NS).

:- pce_end_class(rdf_namespace_browser).
