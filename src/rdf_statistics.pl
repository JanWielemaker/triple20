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

:- pce_begin_class(rdf_statistics_dialog, dialog,
		   "Show statistics").

initialise(D) :->
	send_super(D, initialise, 'RDF Statistics'),
	send(D, append, new(rdf_statistics_table)),
	send(D, append, button(ok, message(D, destroy))).

:- pce_end_class(rdf_statistics_dialog).


:- pce_begin_class(rdf_statistics_table, tabular,
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

:- pce_end_class(rdf_statistics_table).

