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


		 /*******************************
		 *		PATHS		*
		 *******************************/

:- multifile
	user:file_search_path/2.
:- dynamic
	user:file_search_path/2.

user:file_search_path(semweb,   library(semweb)).

:- retractall(file_search_path(triple20, _)),
   prolog_load_context(directory, Dir),
   asserta(file_search_path(triple20, Dir)),
   asserta(file_search_path(library,  triple20('.'))),
   asserta(file_search_path(ontology, triple20('.'))).

:- load_files([ rdf_base,		% Info on base ontologies
		rdf_file,		% Info on files we manage
		library(rdf),		% parser
		semweb(rdf_db),		% triple store
		semweb(rdfs),		% RDFS rules
		semweb(rdf_edit),	% transactions and changes
		rdf_text,		% basic text representation
		rules,			% rendering rules
		rdfs_explorer,		% visualization
		concur			% concurrency
%		rdf_portray
	      ],
	      [ silent(true)
	      ]).

:- pce_image_directory(triple20(icons)).

:- pce_autoload(ulan_timestamp_object_item,
		library(ulan)).

user:file_search_path(snapshot, user_profile(Dir)) :-
	rdf_snapshot_directory(Dir).

		 /*******************************
		 *	  DEBUGGING STUFF	*
		 *******************************/

%:- rdf_debug(1).		% Print messages

dbg :-
%	debug(render),
	guitracer.

%:- nav.
%:- dbg.

:- catch(['~/.xpcerc'], _, true).


		 /*******************************
		 *	     TOPLEVEL		*
		 *******************************/

go :-
	new(X, rdfs_explorer),
	send(X, open).

%	go(+Argv)
%	
%	Main entry.  Options:
%	
%		<file>.rdfj		Specify a journal file
%		--reset			Ignore existing journal
%		--rdfs			Load RDFS
%		--owl			Load OWL
%		--aat			Load AAT
%		--wn			Load Wordnet
%		--wnrdfs		Load Wordnet as RDFS classes
%		--ulan			Load ULAN
%		--world			Load everything we know
%		<file>.{rdf,rdfs,owl}	Load this file

go(Argv) :-
	memberchk('--help', Argv), !,
	usage,
	halt(0).
go(Argv) :-
	(   select(Journal, Argv, Argv1),
	    file_name_extension(_, rdfj, Journal)
	->  (   select('--reset', Argv1, Argv2)
	    ->	Mode = write
	    ;   Mode = append,
		Argv2 = Argv1
	    ),
	    (   Mode == append,
		exists_file(Journal)
	    ->	JournalLoaded = true
	    ;	true
	    ),
	    (	Mode \== read
	    ->	rdf_ensure_snapshot_directory
	    ;	true
	    ),
	    rdfe_open_journal(Journal, Mode)
	;   Argv2 = Argv
	),
	(   select('--nobase', Argv2, Argv3)
	->  NoBase = true
	;   Argv3 = Argv2
	),
	(   JournalLoaded == true
	->  true
	;   rdfe_transaction(parse_argv(Argv3), load_argv),
	    (	NoBase == true
	    ->	true
	    ;   rdfe_transaction(forall(required_base_ontology(O),
					load_base_ontology(O)),
				 required_base_ontologies)
	    )
	),
	new(X, rdfs_explorer),
	send(X, open).

parse_argv([]).
parse_argv(['--rdfs'|T]) :-
	load_base_ontology(rdfs),
	parse_argv(T).
parse_argv(['--owl'|T]) :-
	load_base_ontology(owl),
	parse_argv(T).
parse_argv(['--world'|T]) :-
	load_base_ontology(world),
	parse_argv(T).
parse_argv(['--aat'|T]) :-
	load_base_ontology(aat),
	parse_argv(T).
parse_argv(['--wn'|T]) :-
	load_base_ontology(wn),
	parse_argv(T).
parse_argv(['--wnrdfs'|T]) :-
	load_base_ontology(wnrdfs),
	parse_argv(T).
parse_argv(['--ulan'|T]) :-
	load_base_ontology(ulan),
	parse_argv(T).
parse_argv([File|T]) :-
	file_name_extension(_, Ext, File),
	rdf_file_extension(Ext, _Name),
	Ext \== rdfj, !,
	rdfe_load(File),
	parse_argv(T).
parse_argv(_) :-
	usage,
	halt(1).

usage :-
	print_message(informational, rdf(usage)).

		 /*******************************
		 *	 TURN TO PROGRAM	*
		 *******************************/

save(X) :-
	qsave_program(X, []).

winmain :-
	current_prolog_flag(argv, Argv),
	append(_, [--,Assoc], Argv),
	go([Assoc]).


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(rdf(usage)) -->
	[ 'Usage: ~w [option ...] file ...'-[Me], nl, nl,
	  '  Options:', nl,
	  '    --help              Print usage', nl,
	  '    --reset             Overwrite journal instead of append', nl,
	  '    --nobase            Do NOT load rdfs.rdfs and owl.owl', nl,
	  '    --world             Load the whole world', nl,
	  '    --aat               Load AAT', nl,
	  '    --wn                Load WordNet', nl,
	  '    --wnrdfs            Load WordNet as RDFS Classes', nl,
	  '    --ulan              Load ULAN', nl, nl,
	  '  Files:', nl,
	  '    file.rdf            Load RDF file', nl,
	  '    file.rdfs           Load RDFS file', nl,
	  '    file.owl            Load OWL file', nl,
	  '    file.rdfj           Append/overwrite journal file'
	],
	{ current_prolog_flag(argv, Argv),
	  (   append(_, ['-s',Path|_], Argv)
	  ->  true
	  ;   Argv = [Path|_]
	  ),
	  file_base_name(Path, Me)
	}.
