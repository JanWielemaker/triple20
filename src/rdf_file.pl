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
	    rdf_install_archive/1	% +File
	  ]).
:- use_module(semweb(rdf_edit)).
:- use_module(library(debug)).

%	rdf_file_extension(?Ext, ?Description)

rdf_file_extension(rdf,  'RDF file').
rdf_file_extension(rdfs, 'RDF Schema file').
rdf_file_extension(owl,  'OWL ontology file').
rdf_file_extension(rdfj, 'OntoShow project file').

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
