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
	    rdf_ensure_snapshot_directory/0
	  ]).

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
	absolute_file_name(snapshot(.),
			   [ file_type(directory),
			     access(write),
			     file_errors(fail)
			   ],
			   Dir), !,
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
	(   delete(Local, Dirs, TheDirs)
	;   TheDirs = Dirs
	),
	member(Dir, TheDirs),
	catch(make_directory(Dir), _, fail),
	debug(snapshot, 'Created snapshot directory ~w', [Dir]).
