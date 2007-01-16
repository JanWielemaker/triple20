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

:- module(rdf_dialog, []).
:- use_module(library(pce)).
:- use_module(library(pce_util)).
:- use_module(library(pce_identifier_item)).
:- use_module(library(hyper)).
:- use_module('semweb/rdf_edit').
:- use_module('semweb/rdf_db').
:- use_module('semweb/rdfs').
:- use_module(library(lists)).
:- use_module(rdf_util).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Provide some standard dialogs for ontology handling.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

		 /*******************************
		 *	       SEARCH		*
		 *******************************/

:- pce_begin_class(rdf_search_dialog, dialog,
		   "Search for resources based on textual attributes").

initialise(D) :->
	send_super(D, initialise, 'Search resources'),
	send(D, pen, 0),
	send(D, border, size(2, 10)),
	send(D, append, new(Fields, rdf_search_prop_menu)),
	send(D, append, new(For, label(for, 'For', bold)), right),
	send(For, alignment, left),
	send(D, append, new(SI, rdf_search_text_item(search))),
	send(SI, show_label, @off),
	send(D, append, new(ST, menu(how, cycle)), right),
	send_list(ST, append, [substring, word, prefix, exact]),
	send(ST, show_label, @off),
	send(D, append,
	     new(Find, button(find,
			      message(D, find,
				      SI?selection, ST?selection,
				      Fields?selection))),
	     right),
	send(Find, recogniser,
	     click_gesture(left, s, single,
			   message(D, find,
				   SI?selection, ST?selection,
				   Fields?selection,
				   add))),
	send(D, search_field, '*', @on),
	send(D, search_field, rdfs:label),
	send(D, search_field, rdfs:comment),
	send(Fields, append, resource),
	send_list([Fields, For], alignment, left),
	send(Find, default_button, @on),
	send(Find, active, @off),
					% HACK: force stretching
	send(D, resize_message,
	     message(D, resize_dialog, @arg2)).

search_field(D, Field:prolog, Selected:[bool]) :->
	"Define a field for textual search"::
	get(D, member, search_in, Menu),
	(   Field == '*'
	->  (   get(Menu, member, *, MI)
	    ->  true
	    ;   send(Menu, append,
		     new(MI, menu_item('*', @default, 'Any')))
	    )
	;   rdf_global_id(Field, Global),
	    rdfs_label(Global, Label),
	    (   get(Menu, member, Global, MI)
	    ->  true
	    ;   send(Menu, append,
		     new(MI, menu_item(Global, @default, Label)))
	    )
	),
	(   Selected == @on
	->  send(MI, selected, Selected)
	;   true
	).

selected_predicates(D, Selected:'chain|{*}') :<-
	"Get chain with fields we are searching in"::
	get(D, member, search_in, Menu),
	get(Menu, selection, Selection),
	(   send(Selection, member, '*')
	->  Selected = '*'
	;   Selected = Selection
	).

find(D, What:string, How:name, Fields:chain, Join:[name]) :->
	"Handle clicking the find button"::
	(   send(Fields, member, '*')
	->  Target = '*'
	;   Target = Fields
	),
	send(D?frame, find, What, How, Target, Join).

%	->resize_dialog
%	
%	Properly spread the items of the row holding the text-item,
%	menu and button.  Something XPCE should be able to handle
%	using the declarative layout, but this doesn't work.  Ugly
%	but efficient as long as we can't do better.

resize_dialog(D, Size:size) :->
	object(Size, size(W,_H)),
	get(D, border, size(BW,_)),
	get(D, gap, size(GW,_)),
	send(D, layout, Size),
	get(D, member, find, Find),
	get(D, member, how, How),
	get(D, member, search, TI),
	(   get(D, member, cancel, C)
	->  right_to_left([C,Find,How,TI], GW, W-BW)
	;   right_to_left([Find,How,TI], GW, W-BW)
	).

right_to_left([], _, _).
right_to_left([T], _, R) :- !,
	send(T, right_side, R).
right_to_left([H|T], G, R) :-
	get(H, width, W),
	X is R-W,
	send(H, x, X),
	R2 is X - G,
	right_to_left(T, G, R2).

:- pce_end_class(rdf_search_dialog).


:- pce_begin_class(rdf_search_prop_menu, menu,
		   "Select properties for searching").

variable(saved_selection, chain*, get, "Selection saved over `any'").

initialise(M) :->
	send_super(M, initialise, search_in, toggle,
		   message(@receiver, user_selected, @arg1, @arg2)),
	send(M, layout, horizontal).

user_selected(M, MI:menu_item, Val:bool) :->
	"Deal with search any/specific"::
	(   get(MI, value, '*')
	->  (   Val == @on
	    ->  get(M?selection, clone, Saved),
		send(Saved, delete_all, '*'),
	        send(M, slot, saved_selection, Saved),
		send(M, selection, chain(*)),
		send(M, report, status, 'Searching all literal properties')
	    ;   get(M, saved_selection, Saved),
		Saved \== @nil
	    ->  send(M, selection, Saved)
	    ;   true
	    )
	;   send(M, selected, '*', @off)
	).

:- pce_end_class(rdf_search_prop_menu).



:- pce_begin_class(rdf_search_text_item, text_item,
		   "Text item for entering search strings").

typed(TI, Ev:'event|event_id') :->
	send_super(TI, typed, Ev),
	ignore(send(TI, classify, TI?value_text?string)).

classify(TI, Typed:name) :->
	"Classify the typed value"::
	(   concat_atom([NS,SearchFor], :, Typed),
	    rdf_db:ns(NS, Prefix)
	->  true
	;   SearchFor = Typed,
	    Prefix = ''
	),
	get(TI?device, selected_predicates, P),
	(   P == '*'
	->  (   rdf_has(Subject, _, literal(SearchFor)),
	        sub_atom(Subject, 0, _, _, Prefix)
	    ->  send(TI, match_type, exact)
	    ;   send(TI, match_type, none)
	    )
	;   chain_list(P, Fields),
	    (   member(Field, Fields),
		(   Field == resource
		->  rdf_db:ns(NS, _),
		    rdf_global_id(NS:SearchFor, Subject),
		    rdf(Subject, _, _)
		;   rdf_has(Subject, Field, literal(SearchFor)),
		    sub_atom(Subject, 0, _, _, Prefix)
		)
	    ->  send(TI, match_type, exact)
	    ;   send(TI, match_type, none)
	    )
	).
	
match_type(TI, Type:name) :->
	"Indicate type of match using colour"::
	match_colour(Type, Colour), !,
	send(TI, colour, Colour).

match_colour(exact, blue).
match_colour(_,     black).

:- pce_end_class.


:- pce_begin_class(rdf_language_item, text_item,
		   "Select a language code").


:- pce_global(@iso_639_value_set, make_iso_639_value_set).

make_iso_639_value_set(Set) :-
	new(Set, chain),
	setof(L, C^iso_639(C, L), Ls),
	forall(member(L, Ls), send(Set, append, L)),
	setof(C, C^iso_639(C, L), Cs),
	forall(member(C, Cs), send(Set, append, C)).

initialise(I, Name:name, Sel:[name], Msg:[code]*) :->
	send_super(I, initialise, Name, @default, Msg),
	(   Sel \== @default
	->  send(I, selection, Sel)
	;   true
	),
	send(I, value_set, @iso_639_value_set).

selection(I, C:name) :->
	"Set selection from code"::
	send_super(I, selection, C),
	(   iso_639(C, L)
	->  send(I?value_text, string, string('%s (%s)', C, L)),
	    send(I, caret, 0)
	;   true
	).

selection(I, Sel:name) :<-
	"Get language code"::
	get_super(I, selection, S0),
	(   iso_639(S0, L)
	->  Sel = S0,
	    send(I?value_text, string, string('%s (%s)', S0, L))
	;   iso_639(Sel, S0)
	->  send(I?value_text, string, string('%s (%s)', Sel, S0))
	;   Sel = S0,
	    send(I, report, status, 'Unknown language: %s', Sel)
	).

:- pce_end_class(rdf_language_item).


		 /*******************************
		 *	  GENERIC DIALOG	*
		 *******************************/

:- pce_begin_class(rdf_dialog, dialog,
		   "Generic parts").

initialise(D, Client:client=[visual], Label:label=[name]) :->
	send_super(D, initialise, Label),
	send(D, client, Client).

client(D, For:[visual]) :->
	(   For == @default
	->  true
	;   new(_, partof_hyper(For, D, dialog, client))
	).
	    
open(D, Pt:[point]) :->
	(   Pt = @default,
	    get(D, hypered, client, Client),
	    (	send(Client, instance_of, frame)
	    ->  send(D, transient_for, Client),
		send(D, modal, transient),
		send_super(D, open_centered, Client?area?center)
	    ;   get(Client, frame, Frame),
		(   send(Client, has_get_method, display_position)
		->  get(Client, display_position, point(X, Y))
		;   X = 0, Y = 0
		),
		get(Client?area, height, H)
	    ->  send(D, transient_for, Frame),
		send(D, modal, transient),
		send_super(D, open, point(X, Y+H+5))
	    )
	;   send_super(D, open, Pt)
	).

confirm(D, Pt:[point], Rval:any) :<-
	send(D, open, Pt),
	get_super(D, confirm, Rval).

item_selection(F, From:name, Warn:[bool], Selection:any) :<-
	"Get selection of an item"::
	get(F, member, From, Item),
	(   Warn == @off
	->  pce_catch_error(_, get(Item, selection, Selection))
	;   get(Item, selection, Selection)
	).

standard_buttons(F, Action:'name|button') :->
	"Append action and cancel buttons"::
	(   atom(Action)
	->  new(B, button(Action))
	;   B = Action
	),
	send(F, append, B),
	send(B, active, @off),
	send(B, default_button, @on),
	send(F, append, button(cancel)).

cancel(F) :->
	send(F, destroy).

:- pce_end_class(rdf_dialog).



		 /*******************************
		 *	       FILE		*
		 *******************************/

%	@rdf_source_file_type
%	
%	This type enumerates all registered RDF sourcefiles.

make_rdf_source_file_type :-
	(   object(@rdf_source_file_type)
	->  true
	;   new(@rdf_source_file_type,
		type(rdf_source_file, value_set,
		     quote_function(@prolog?rdf_source_files),
		     @nil))
	).

rdf_source_files(Chain) :-
	new(Chain, chain(user)),
	forall(rdf_source(File), send(Chain, append, File)),
	send(Chain, sort).

:- initialization
   make_rdf_source_file_type.

:- pce_begin_class(rdf_merge_file_dialog, dialog,
		   "Prompt for merging two files").

initialise(D) :->
	send_super(D, initialise, 'Merge files'),
	send(D, append, new(From, text_item(from))),
	send(D, append, button('->', @nil)),
	send(D, append, new(Into, text_item(into))),
	send_list([From, Into],
		  [ type(@rdf_source_file_type),
		    length(60)
		  ]),
	send(From, type, @rdf_source_file_type),
	send(D, append, new(Merge, button(merge))),
	send(Merge, active, @off),
	send(Merge, default_button, @on),
	send(D, append, button(cancel)).

modified_item(D, _Gr:graphical, _M:bool) :->
	get(D, member, merge, Button),
	(   get(D, member, into, IntoItem),
	    get(D, member, from, FromItem),
	    pce_catch_error(_, get(IntoItem, selection, Into)),
	    pce_catch_error(_, get(FromItem, selection, From)),
	    Into \== From
	->  send(Button, active, @on)
	;   send(Button, active, @off)
	).

cancel(D) :->
	send(D, destroy).

merge(D) :->
	get(D, member, into, IntoItem),
	get(D, member, from, FromItem),
	get(IntoItem, selection, Into),
	get(FromItem, selection, From),
	rdf_merge_files(Into, From),
	send(D, destroy).

:- pce_end_class.


		 /*******************************
		 *	       RESOURCE		*
		 *******************************/

:- pce_begin_class(rdf_rename_dialog, rdf_dialog,
		   "Rename a resource").

initialise(D, Resource:name, Client:[graphical]) :->
	send_super(D, initialise, Client, 'Rename resource'),
	send(D, append, new(From, identifier_item(from, Resource))),
	send(From, editable, @off),
	send(D, append, new(To, identifier_item(into, Resource))),
	send_list([From, To], length(40)),
	send(D, standard_buttons, rename).

modified_item(D, _Gr:graphical, _M:bool) :->
	get(D, member, rename, Button),
	(   get(D, item_selection, from, @off, From),
	    get(D, item_selection, into, @off, Into),
	    Into \== From
	->  send(Button, active, @on)
	;   send(Button, active, @off)
	).

rename(D) :->
	get(D, item_selection, from, From),
	get(D, item_selection, into, Into),
	rdf_change_resource(From, Into),
	send(D, destroy).

:- pce_end_class(rdf_rename_dialog).


:- pce_begin_class(t20_new_namespace_dialog, rdf_dialog,
		   "Define a new namespace abbreviation").

initialise(D, Client:[visual], DefID:[name], DefURI:[name]) :->
	"Prompt for a new namespace"::
	send_super(D, initialise, Client, 'Define new namespace'),
	send(new(report_dialog), below, D),
	send(D, append, new(IDItem, identifier_item(id, DefID))),
	send(D, append, new(URIItem, identifier_item(uri, DefURI)), right),
	send(D, append, button(create, message(D, return, ok))),
	send(D, append, button(cancel)),
	send(IDItem, length, 6),
	send(URIItem, length, 50),
	(   DefURI == @default
	->  send(URIItem, selection, 'http://')
	;   true
	),
	send(D, default_button, create).

run(D) :->
	"Run the dialog"::
	get(D, member, id, IDItem),
	get(D, member, uri, URIItem),
	repeat,
	(   get(D, confirm, _Reply)
	->  get(IDItem, selection, ID),
	    get(URIItem, selection, URI),
	    catch(rdfe_transaction(register_ns(ID, URI),
				   define_namespace),
		  E, true),
	    (   var(E)
	    ->  !, send(D, destroy)
	    ;   message_to_string(E, Message),
		send(D, report, error, Message),
		fail
	    )
	;   !
	).

register_ns(ID, URI) :-
	(   xml_name(ID)
	->  true
	;   throw(error(type_error(xml_name, ID), _))
	),
	(   (   sub_atom(URI, _, _, 0, #)
	    ;	sub_atom(URI, _, _, 0, /)
	    )
	->  true
	;   throw(error(type_error(namespace, URI),
			context(_, 'Namespace URI must end in # or /')))
	),
	rdfe_register_ns(ID, URI).

:- pce_end_class(t20_new_namespace_dialog).



		 /*******************************
		 *	  IMAGE_BUTTON		*
		 *******************************/

:- pce_begin_class(image_button, bitmap,
		   "Button created from an image").

variable(message,    code*, get, "Associated message").

initialise(IB, Image:image, Message:[code], Balloon:[char_array]) :->
	send_super(IB, initialise, Image),
	(   Message \== @default
	->  send(IB, slot, message, Message)
	;   true
	),
	(   Balloon \== @default
	->  send(IB, help_message, tag, Balloon)
	;   true
	).

:- pce_global(@image_button_recogniser,
	      new(click_gesture(left, '', single,
				message(@receiver, execute)))).

event(IB, Ev:event) :->
	(   send_super(IB, event, Ev)
	->  true
	;   send(@image_button_recogniser, event, Ev)
	).

execute(IB) :->
	(   get(IB, message, Message),
	    Message \== @nil
	->  send(Message, forward_receiver, IB)
	;   true
	).

:- pce_end_class(image_button).
