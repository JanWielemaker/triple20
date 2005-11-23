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

:- module(t20wn, []).
:- use_module(triple20(rdf_rules)).
:- use_module(triple20(rdf_util)).
:- use_module(triple20(rdf_cache)).
:- use_module(library('semweb/rdfs')).
:- use_module(library('semweb/rdf_db')).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Triple20 plugin for the SKOS  framework   for  thesauri. It defines skos
specific expansion of  the  hierarchy  as   well  as  editing  the  SKOS
hierarchy.

Issues:
	- Menus for creating new individuals, etc.
	- Icons for skos class, part and individual.

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- rdf_register_ns(wn2,
		   'http://wordnet.princeton.edu/wn#').

:- multifile
	rdf_default_rules:resource/3.

rdf_default_rules:resource(wnclass,     image, image('16x16/wnclass.xpm')).

:- begin_rules(display, wordnet).

		 /*******************************
		 *	       LABELS		*
		 *******************************/

label_class(Obj, wn_class_label) :-
	rdfs_individual_of(Obj, wn2:'Synset'), !.
label_class(Obj, Class) :-
	super::label_class(Obj, Class).

icon_resource(R, wnclass) :-
	rdfs_individual_of(R, wn2:'Synset'), !.
icon_resource(R, Resource) :-
	super::icon_resource(R, Resource).

		 /*******************************
		 *	     HIERARCHY		*
		 *******************************/

child_cache(R, Cache, rdf_node) :-
	rdfs_individual_of(R, wn2:'Synset'),
	rdf_cache(lsorted(V), rdf_has(V, wn2:hyponymOf, R), Cache).
child_cache(R, Cache, Class) :-
	super::child_cache(R, Cache, Class).

parent(R, Parent, rdf_node) :-
	rdfs_individual_of(R, wn2:'Synset'),
	rdf(R, wn2:hyponymOf, Parent).
parent(R, Parent, Class) :-
	super::parent(R, Parent, Class).


		 /*******************************
		 *	   DRAG AND DROP	*
		 *******************************/

%	drop_resource_command(+Onto, +Drop, -Command)
%	
%	Determine the command(s) to execute if an object representing Drop
%	is dropped onto an object representing Ondo.  

/*
drop_resource_command(C, R, Command) :-
	rdfs_individual_of(C, wn2:'Synset'),
	skos_concept_relation(Command, _P).
drop_resource_command(C, R, Command) :-
	super::drop_resource_command(C, R, Command).

drop_resource(Command, C, R) :-
	skos_concept_relation(Command, Property), !,
	rdf_set_object(R, Property, C).
drop_resource(Command, C, R) :-
	super::drop_resource(Command, C, R).
*/

:- end_rules.
