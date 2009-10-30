/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2005, University of Amsterdam

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

:- module(t20_plugin,
	  [ load_plugins/0,		% Load registered plugins
	    scan_plugins/0,		% Scan for Triple20 plugins
	    save_plugins/0,		% Save plugin config
	    plugin_dir/2		% ?Id, -Path
	  ]).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).
:- use_module(library(lists)).

:- dynamic
	plugin_rdf_file/1.
:- volatile
	plugin_rdf_file/1.

%	plugin_dir(?Id, -Path).
%
%	Enumerate the locations of the plugin directories.

plugin_dir(local, '.').
plugin_dir(user,  user_profile(Base)) :-
	(   current_prolog_flag(windows, true)
	->  Base = 'Triple20'
	;   Base = '.triple20'
	).
plugin_dir(system, triple20('../Plugins')).

%	load_plugin_config/0
%
%	Load the plugin configuration. The  configuration is loaded from
%	the  first  location.  See  search  path  t20plugin  defined  in
%	load.pl. We do not load from a file   because we do not want the
%	files to endup in the file administration.

load_plugin_config :-
	absolute_file_name(t20plugin(plugins), Path,
			   [ extensions([rdf,ttl]),
			     access(read),
			     file_errors(fail)
			   ]), !,
	file_name_extension(_, Ext, Path),
	ext_format(Ext, Format),
	open(Path, read, In, [type(binary)]),
	call_cleanup(rdf_load(stream(In),
			      [ format(Format),
				db(triple20)
			      ]),
		     close(In)),
	assert(plugin_rdf_file(Path)),
	rdf_load(ontology('t20.rdfs')).
load_plugin_config.

ext_format(ttl, turtle) :- !.
ext_format(_,   xml).

%	load_plugins/0
%
%	Load current plugin configuration.

load_plugins :-
	load_plugin_config,
	(   rdfs_individual_of(P, t20:'Plugin'),
	    rdf_has(P, t20:active, t20:true),
	    rdf_has(P, t20:source, literal(Source)),
	    catch(load_plugin(Source), E,
		  print_message(error, E)),
	    fail
	;   true
	).

load_plugin(Source) :-
	use_module(user:t20plugin(Source), []).

%	save_plugins/0
%
%	Save plugin information. If writeable, the  info is saved to the
%	file it is loaded  from.  If   not  writeable,  another  file is
%	searched that is writeable.

save_plugins :-
	plugin_rdf_file(Path),
	(   rdf_modified(triple20)
	->  true
	;   access_file(Path, write)
	->  rdf_save(Path, [db(triple20)])
	;   absolute_file_name(t20plugin(plugins),
			       Save,
			       [ extensions([rdf]),
				 access(write),
				 file_errors(fail)
			       ])
	->  rdf_save(Save, [db(triple20)])
	).

rdf_modified(DB) :-
	rdf_md5(DB, MD5),
	rdf_db:rdf_source(DB, _URL, _Time, _Triples, UnmodifiedMD5),
	MD5 \== UnmodifiedMD5.


		 /*******************************
		 *	     SCANNING		*
		 *******************************/

%	scan_plugins
%
%	Scan plugin directories for defined plugins  and add them to the
%	RDF descriptions.

scan_plugins :-
	(   plugin_dir(Id, Spec),
	    absolute_file_name(Spec, Dir,
			       [ file_type(directory),
				 access(read),
				 file_errors(fail)
			       ]),
	    scan_plugin_dir(Id, Dir),
	    fail
	;   true
	).


%	scan_plugin_dir(+Dir)
%
%	Scan a directory for  Prolog  files   that  look  like  Triple20
%	plugins. Create a description for them in the RDF database.

scan_plugin_dir(Id, Dir) :-
	atom_concat(Dir, '/*.pl', Pattern),
	expand_file_name(Pattern, Files),
	(   member(File, Files),
	    catch(open(File, read, In), _, fail),
	    call_cleanup(read_plugin_info(In, Attributes), close(In)),
	    register_plugin(File, Id, Attributes),
	    fail
	;   true
	).

read_plugin_info(In, Attributes) :-
	between(1, 10, _),
	catch(read(In, Term), _, fail),
	Term = (:- plugin(Attributes)), !.

register_plugin(File, Id, Attributes) :-
	DB = triple20,
	plugin_class(Id, Class),
	file_base_name(File, Source),
	rdf_global_term(Attributes, Global),
	(   rdfs_individual_of(R, Class),
	    rdf_has(R, t20:source, literal(Source))
	->  update_plugin(Global, R)
	;   rdf_bnode(Plugin),
	    rdf_assert(Plugin, rdf:type, Class, DB),
	    rdf_assert(Plugin, t20:source, literal(Source)),
	    update_plugin(Global, Plugin)
	).

update_plugin([], _).
update_plugin([P=V|T], R) :-
	mkliteral(V, O),
	(   rdf_has(R, P, O)
	->  true
	;   rdf_retractall(R, P, _),
	    rdf_assert(R, P, O, triple20)
	),
	update_plugin(T, R).

mkliteral(Atom, literal(Atom)) :-
	atom(Atom), !.
mkliteral(String, literal(Atom)) :-
	is_string(String), !,
	atom_codes(Atom, String).

is_string([]).
is_string([H|T]) :-
	integer(H),
	between(0, 256, H),		% TBD: Unicode?
	is_string(T).

term_expansion(plugin_class(Id, C0),
	       plugin_class(Id, C)) :-
	rdf_global_id(C0, C).

plugin_class(local,  t20:'LocalPlugin').
plugin_class(user,   t20:'UserPlugin').
plugin_class(system, t20:'SystemPlugin').
