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
   asserta(file_search_path(ontology_root, triple20('../Ontologies'))).

:- load_files([ rdf_base,		% Info on base ontologies
		rdf_file,		% Info on files we manage
		library(rdf),		% parser
		semweb(rdf_db),		% triple store
		semweb(rdfs),		% RDFS rules
		semweb(rdf_edit),	% transactions and changes
		owl,			% OWL inferencing
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
		 *	      VERSION		*
		 *******************************/

rdf_version('0.3, November 2003').
required_prolog_version(50211).

check_prolog_version :-
	current_prolog_flag(version, MyVersion),
	required_prolog_version(Required),
	(   MyVersion >= Required
	->  true
	;   user_version(MyVersion, MyV),
	    user_version(Required, Req),
	    send(@display, confirm,
		 'This version of Triple20 requires SWI-Prolog %s\n\
		  while you are running %s.  Triple20 may not function\n\
		  properly.', Req, MyV)
	->  true
	;   halt
	).

user_version(N, Version) :-
	Major is N // 10000,
	Minor is (N // 100) mod 100,
	Patch is N mod 100,
	concat_atom([Major, Minor, Patch], '.', Version).


		 /*******************************
		 *	     TOPLEVEL		*
		 *******************************/

go :-
	go([]).

%	go(+Argv)
%	
%	Main entry.  Options:
%	
%		<file>.rdfj		Specify a journal file
%		--base			List base ontologies
%		--base=Base		Load base ontology
%		<file>.{rdf,rdfs,owl}	Load this file

go(Argv) :-
	memberchk('--help', Argv), !,
	usage,
	halt(0).
go(Argv) :-
	debug(cache),
	check_prolog_version,
	rdf_prepare_ontology_dirs,
	debug_options(Argv, Argv0),
	(   select(Journal, Argv0, Argv1),
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
parse_argv(['--base'|_]) :- !,
	(   current_base_ontology(Base),
	    writeln(Base),
	    fail
	;   true
	),
	halt(0).
parse_argv([Cmd|T]) :-
	atom_concat('--base=', Base, Cmd), !,
	load_base_ontology(Base),
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

debug_options([], []).
debug_options([H|T0], T) :-
	atom_concat('--debug=', Base, H), !,
	debug(Base),
	debug_options(T0, T).
debug_options([H|T0], [H|T]) :-
	debug_options(T0, T).


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
	  '    --base              List known base ontologies', nl,
	  '    --base=Base         Load base ontology', nl,
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
