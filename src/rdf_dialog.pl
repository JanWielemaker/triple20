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


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Provide some standard dialogs for ontology handling.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(rdf_search_dialog, dialog,
		   "Search for resources based on textual attributes").

initialise(D) :->
	send_super(D, initialise, 'Search resources'),
	send(D, pen, 0),
	send(D, border, size(2, 10)),
	send(D, append, new(Fields, menu(search_in, toggle))),
	send(D, append, new(For, label(for, 'For', bold)), right),
	send(For, alignment, left),
	send(D, append, new(SI, text_item(search))),
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
	send(D, search_field, rdfs:label, @on),
	send(D, search_field, rdfs:comment),
	send(Fields, append, resource),
	send(Fields, layout, horizontal),
	send_list([Fields, For], alignment, left),
	send(Find, default_button, @on),
	send(Find, active, @off),
					% HACK: force stretching
	send(D, resize_message,
	     message(D, resize_dialog, @arg2)).

search_field(D, Field:prolog, Selected:[bool]) :->
	"Define a field for textual search"::
	get(D, member, search_in, Menu),
	rdf_global_id(Field, Global),
	rdfs_label(Global, Label),
	send(Menu, append,
	     new(MI, menu_item(Global, @default, Label))),
	(   Selected == @on
	->  send(MI, selected, Selected)
	;   true
	).

find(D, What:string, How:name, Fields:chain) :->
	send(D?frame, find, What, How, Fields).

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
	get(Find, width, FW),
	get(How, width, HW),
	FX is W-BW-FW-4,
	HX is FX-GW-HW,
	send(Find, x, FX),
	send(How, x, HX),
	send(TI, right_side, HX-GW).

:- pce_end_class(rdf_search_dialog).
