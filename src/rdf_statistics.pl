/*  File:    rdf_statistics.pl
    Author:  Jan Wielemaker
    Created: Feb  4 2003
    Purpose: Display statistics
*/

:- module(rdf_statistics,
	  [
	  ]).
:- use_module(library(pce)).
:- use_module(library(tabular)).
:- use_module(library(rdf_db)).
:- use_module(rdf_edit).

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
	send(ST, next_row),
	flag(rdf_triples, Old, 0),
	(   rdf_source(Source),
	    rdf_statistics(triples_by_file(Source, Triples)),
	    flag(rdf_triples, C, C+Triples),
	    send(ST, show_source, Source),
	    fail
	;   flag(rdf_triples, Total, Old)
	),
	send(ST, append, 'Total from files:', bold, halign := right),
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
	send(ST, next_row).

:- pce_end_class(rdf_statistics_table).

