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


:- module(triple20,
	  [ triple20/0,
	    triple20/1,			% +Argv
					% Misc
	    t20/1,			% +Action
					% Maintenance
	    t20_save/1,			% +File

	    t20_winmain/0		% Windows toplevel
	  ]).
	    


		 /*******************************
		 *		PATHS		*
		 *******************************/

:- multifile
	user:file_search_path/2.
:- dynamic
	user:file_search_path/2.

user:file_search_path(semweb,   library(semweb)).

:- user:(retractall(file_search_path(triple20, _)),
	 prolog_load_context(directory, Dir),
	 asserta(file_search_path(triple20, Dir)),
	 asserta(file_search_path(library,  triple20('.'))),
	 asserta(file_search_path(ontology_root, triple20('../Ontologies')))).

:- load_files([ rdf_base,		% Info on base ontologies
		rdf_file,		% Info on files we manage
		library(rdf),		% parser
		semweb(rdf_db),		% triple store
		semweb(rdfs),		% RDFS rules
		semweb(rdf_edit),	% transactions and changes
		library(broadcast),	% Broadcasting service
		owl,			% OWL inferencing
		rdf_text,		% basic text representation
		rules,			% rendering rules
		anon,			% Nicely show anonymous objects
		rdfs_explorer		% visualization
%		rdf_portray
	      ],
	      [ silent(true)
	      ]).
:- ( exists_directory(plugins) ->           %added plugins directory BJW
      expand_file_name('plugins/*.pl', Files),
      load_files(Files, [])
   ; true).

:- pce_image_directory(triple20(icons)).

:- pce_autoload(ulan_timestamp_object_item,
		library(ulan)).

user:file_search_path(snapshot, user_profile(Dir)) :-
	rdf_snapshot_directory(Dir).


		 /*******************************
		 *	      VERSION		*
		 *******************************/

t20_version('0.71, May 2005').
required_prolog_version(50518).

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

triple20 :-
	triple20([]).

%	triple20(+Argv)
%	
%	Main entry.  Options:
%	
%		<file>.rdfj		Specify a journal file
%		--base			List base ontologies
%		--base=Base		Load base ontology
%		<file>.{rdf,rdfs,owl}	Load this file

%triple20(_Argv) :-
%	protocol('triple20.log'),
%	gtrace,
%	fail.
triple20(Argv) :-
	memberchk('--help', Argv), !,
	usage,
	halt(0).
triple20(Argv) :-
	debug(cache),
	check_prolog_version,
	debug_options(Argv, Argv0a),
	load_plugins(Argv0a, Argv0),
	(   select(OSJournal, Argv0, Argv1),
	    file_name_extension(_, rdfj, OSJournal)
	->  prolog_to_os_filename(Journal, OSJournal),
	    (   select('--reset', Argv1, Argv2)
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
	;   Argv2 = Argv0
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
	absolute_file_name(File, AbsName),
	(   \+ access_file(File, exist)
	->  rdf_save(File, AbsName)
	;   true
	),
	rdfe_load(AbsName, [namespaces(NSList)]),
	register_default_ns(AbsName, NSList),
	(   access_file(File, write)
	->  rdfe_set_file_property(File, default(all))
	;   true
	),
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


load_plugins([], []).
load_plugins([File|T0], T) :-
	file_name_extension(_, pl, File), !,
	load_files([File]),
	load_plugins(T0, T).
load_plugins([H|T0], [H|T]) :-
	load_plugins(T0, T).


usage :-
	print_message(informational, rdf(usage)).


		 /*******************************
		 *	    SETUP PATHS		*
		 *******************************/

:- initialization
	rdf_prepare_ontology_dirs.


		 /*******************************
		 *	 PUBLIC ACTIONS		*
		 *******************************/

%	t20(+Message)
%	
%	Send a message to all Triple20 toplevel windows.  Note that this
%	doesn't send any messages if no window exists.  Useful actions
%	are:
%	
%		* refresh
%		Refresh the interface after substantial changes to the
%		data.

t20(Action) :-
	broadcast(triple20(Action)).


		 /*******************************
		 *	 TURN TO PROGRAM	*
		 *******************************/

t20_save(X) :-
	qsave_program(X, []).

%	t20_winmain/0
%	
%	MS-Windows toplevel when started by clicking a .RDF* file.  See
%	setup.pl for associating .RDF, etc. with Triple20.

t20_winmain :-
	current_prolog_flag(argv, Argv),
	append(_, [--,Assoc], Argv),
	triple20([Assoc]).


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
	  '    file.pl             Load Triple20 (Prolog) plugin', nl,
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
