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


:- module(rdf_statistics,
	  [
	  ]).
:- use_module(library(pce)).
:- use_module(library(tabular)).
:- use_module(semweb(rdf_db)).
:- use_module(semweb(rdf_edit)).

:- pce_begin_class(rdf_statistics_dialog, tabbed_window,
		   "Show various statistics").

initialise(SD) :->
	send_super(SD, initialise, 'RDF Statistics'),
	send(SD, append, new(rdf_file_dialog), files),
	send(SD, append, new(rdf_call_dialog), calls),
	send(new(D2, dialog), below, SD),
	send(D2, resize_message, message(D2, layout, @arg2)),
	send(D2, append, button(ok, message(SD, destroy))).


:- pce_end_class(rdf_statistics_dialog).


		 /*******************************
		 *		FILES		*
		 *******************************/

:- pce_begin_class(rdf_file_dialog, table_window,
		   "Show statistics").

initialise(D) :->
	send_super(D, initialise, 'Loaded files', new(rdf_file_table)).

:- pce_end_class(rdf_file_dialog).


:- pce_begin_class(rdf_file_table, tabular,
		   "Table with statistical information").


initialise(ST) :->
	send_super(ST, initialise),
	send(ST, rules, all),
	send(ST, cell_spacing, -1),
	send(ST, cell_padding, size(5,3)),
	send(ST, show_sources).

clear(ST) :->
	"Delete all rows"::
	send(ST, delete_rows).

show_sources(ST) :->
	send(ST, append, 'Loaded source', bold, center, background := khaki1),
	send(ST, append, 'Triples',       bold, center, background := khaki1),
	send(ST, append, 'Loaded',        bold, center, background := khaki1),
	send(ST, next_row),
	flag(rdf_triples, Old, 0),
	(   rdf_source(Source),
	    rdf_statistics(triples_by_file(Source, Triples)),
	    flag(rdf_triples, C, C+Triples),
	    send(ST, show_source, Source),
	    fail
	;   flag(rdf_triples, Total, Old)
	),
	send(ST, append, 'Total on files:', bold, halign := right),
	send(ST, append, Total, bold, halign := right),
	send(ST, next_row),
	send(ST, append, 'Total:', bold, halign := right),
	rdf_statistics(triples(TotalDB)),
	send(ST, append, TotalDB, bold, halign := right),
	send(ST, next_row).

show_source(ST, Source:name) :->
	send(ST, append, Source),
	rdf_statistics(triples_by_file(Source, Triples)),
	send(ST, append, Triples, halign := right),
	rdf_db:rdf_source(Source, _, Loaded, _MD5),
	send(ST, append, Loaded, halign := right),
	send(ST, next_row).

:- pce_end_class(rdf_file_table).


		 /*******************************
		 *	       CALLS		*
		 *******************************/

:- pce_begin_class(rdf_call_dialog, table_window,
		   "Show call statistics").

initialise(D) :->
	Khaki = (background := khaki1),
	send_super(D, initialise, 'RDF call statistics', new(T, tabular)),
	send(T, rules, all),
	send(T, cell_spacing, -1),
	send(T, cell_padding, size(5,3)),
	send(T, append, 'Indexed', bold, center, Khaki, colspan := 3),
	send(T, append, 'Calls',   bold, center, Khaki,
	     rowspan := 2, valign := center),
	send(T, next_row),
	send(T, append, 'Subject', bold, center, Khaki),
	send(T, append, 'Object', bold, center, Khaki),
	send(T, append, 'Predicate', bold, center, Khaki),
	send(T, next_row),
	(   rdf_statistics(lookup(rdf(S,P,O), Calls)),
	    send(T, append, S, bold, center),
	    send(T, append, P, bold, center),
	    send(T, append, O, bold, center),
	    send(T, append, Calls, normal, right),
	    send(T, next_row),
	    fail
	;   true
	),
	(   rdf_statistics(searched_nodes(Nodes))
	->  send(T, append, 'Searched nodes', bold, right, colspan := 3),
	    send(T, append, Nodes, normal, right),
	    send(T, next_row)
	;   true
	).

:- pce_end_class(rdf_call_dialog).
