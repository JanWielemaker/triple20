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

:- module(rdf_file,
	  [ rdf_file_extension/2,	% Ext, Description
	    rdf_snapshot_directory/1,	% -Dir
	    rdf_ensure_snapshot_directory/0,
	    rdf_archive_journal/1,	% +File
	    rdf_install_archive/1,	% +File
	    rdf_prepare_ontology_dirs/0,

	    rdf_clear_ontology_cache/0,
	    rdf_clear_snapshots/0
	  ]).
:- use_module(library('semweb/rdf_edit')).
:- use_module(library(debug)).
:- use_module(library(lists)).

%	rdf_file_extension(?Ext, ?Description)

rdf_file_extension(rdf,  'RDF file').
rdf_file_extension(rdfs, 'RDF Schema file').
rdf_file_extension(owl,  'OWL ontology file').
rdf_file_extension(ttl,  'Turtle file').
rdf_file_extension(nt,   'N-Triples file').
rdf_file_extension(rdfj, 'Triple20 project file').


		 /*******************************
		 *	    FILE SEARCH		*
		 *******************************/

:- multifile
	user:file_search_path/2.
:- dynamic
	user:file_search_path/2.

%	rdf_prepare_ontology_dirs/0
%
%	Prepare all subdirectories of the expansion of the ontology_root
%	path-alias as ontology directories. It creates a cache directory
%	if allowed and  adds  the  directory   to  the  search  path for
%	ontology.

rdf_prepare_ontology_dirs :-
	(   absolute_file_name(ontology_root('.'),
			       [ file_type(directory),
				 solutions(all)
			       ],
			       Dir),
	    prepare_ontology_root(Dir),
	    fail
	;   true
	).

prepare_ontology_root(Dir) :-
	working_directory(Old, Dir),
	call_cleanup(prepare_ontology_dir(.),
		     working_directory(_, Old)).

prepare_ontology_dir(Dir) :-
	(   user:file_search_path(ontology, ontology_root(Dir))
	->  true
	;   assert(user:file_search_path(ontology, ontology_root(Dir)))
	),
	forall(sub_dir(Dir, Sub),
	       prepare_ontology_dir(Sub)).

sub_dir(Dir, Sub) :-
	(   Dir == '.'
	->  Pattern = '*'
	;   atom_concat(Dir, '/*', Pattern)
	),
	expand_file_name(Pattern, Subs),
	member(Sub, Subs),
	Sub \== 'CVS',
	\+ sub_atom(Sub, 0, _, _, '_'),		% skip Windows hidden files
	\+ sub_atom(Sub, _, _, _, '/_'),
	\+ sub_atom(Sub, 0, _, _, '.'),		% skip Unix hidden files
	\+ sub_atom(Sub, _, _, _, '/.'),
	\+ sub_atom(Sub, _, _, 0, '/CVS'),	% skip CVS directories
	exists_directory(Sub).


		 /*******************************
		 *	    CLEAR CACHE		*
		 *******************************/

rdf_clear_ontology_cache :-
	(   absolute_file_name(ontology(.),
			       [ file_type(directory),
				 access(write),
				 solutions(all),
				 file_errors(fail)
			       ],
			       Dir),
	    atom_concat(Dir, '/*.trp', Pattern),
	    expand_file_name(Pattern, Files),
	    forall(member(F, Files),
		   delete_file(F)),
	    fail
	;   true
	).


		 /*******************************
		 *	    SNAPSHOTS		*
		 *******************************/

%	snapshot_directory(-Dir)
%
%	Basename for directory to use for snapshots.

rdf_snapshot_directory(Dir) :-
	(   current_prolog_flag(unix, true)
	->  Dir = '.triple20'
	;   Dir = 'Triple20'
	).

%	rdf_ensure_snapshot_directory
%
%	Ensure the existence of a directory for storing snapshots. On
%	Unix the preferred location is ~/.triple20.  On Windows
%	%USERPROFILE%\Triple20.  If necessary the local directory will
%	be used.

rdf_ensure_snapshot_directory :-
	snapshot_directory(Dir), !,
	debug(snapshot, 'Using snapshot directory ~w', [Dir]).
rdf_ensure_snapshot_directory :-
	findall(Dir,
		absolute_file_name(snapshot(.),
				   [ file_type(directory),
				     file_errors(fail),
				     solutions(all)
				   ],
				   Dir),
		Dirs),
	rdf_snapshot_directory(Base),
	absolute_file_name(Base, Local),
	(   delete(Dirs, Local, TheDirs)
	;   TheDirs = Dirs
	),
	member(Dir, TheDirs),
	catch(make_directory(Dir), _, fail),
	debug(snapshot, 'Created snapshot directory ~w', [Dir]).

snapshot_directory(Dir) :-
	absolute_file_name(snapshot(.),
			   [ file_type(directory),
			     access(write),
			     file_errors(fail)
			   ],
			   Dir).

%	rdf_clear_snapshots
%
%	Clear the contents of the  users   snapshot  directory. Use with
%	care as it may make  it  difficult   or  impossible  to  reuse a
%	project file.

rdf_clear_snapshots :-
	(   snapshot_directory(Dir)
	->  atom_concat(Dir, '/*.trp', Pattern),
	    expand_file_name(Pattern, Files),
	    forall(member(F, Files),
		   delete_file(F))
	;   true
	).


		 /*******************************
		 *	      ARCHIVE		*
		 *******************************/

%	rdf_archive_journal(+FileSpec)
%
%	Create, using InfoZip, an archive containing the journal
%	(project) file and all files on which it depends.

rdf_archive_journal(Spec) :-
	absolute_file_name(Spec,
			   [ extensions([zip]),
			     access(write)
			   ],
			   Archive),
	catch(delete_file(Archive), _, true),
	rdfe_current_journal(Journal),
	findall(X, rdfe_snapshot_file(X), SnapShots),
	concat_atom([Journal|SnapShots], '" "', Cmd0),
	sformat(Cmd, 'zip -j "~w" "~w"', [Archive, Cmd0]),
	shell(Cmd).

%	rdf_install_archive(+Spec)
%
%	Extract an archive created with rdf_archive_journal/1 to the
%	proper environment.

rdf_install_archive(Spec) :-
	rdf_ensure_snapshot_directory,
	absolute_file_name(Spec,
			   [ extensions([zip]),
			     access(read)
			   ],
			   Archive),
	snapshot_directory(Dir),
	prolog_to_os_filename(Dir, OsDir),
	prolog_to_os_filename(Archive, OsArchive),
	debug(snapshot, 'Restoring snapshots into ~w', [Dir]),
	sformat(Cmd, 'unzip -qq -n -d "~w" "~w" "*.trp"', [OsDir, OsArchive]),
	shell(Cmd),
	debug(snapshot, 'Restoring journal', []),
	sformat(Cmd2, 'unzip "~w" "*.rdfj"', [OsArchive]),
	shell(Cmd2).
