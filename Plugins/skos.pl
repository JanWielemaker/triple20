/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2005, University of Amsterdam

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

:- module(t20skos, []).
:- include(triple20(plugin)).
:- use_module(triple20(rdf_util)).
:- plugin([ rdfs:label   = 'SKOS',
	    rdfs:comment = 'Vizualise and edit SKOS hierarchies'
	  ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Triple20 plugin for the SKOS   (Simple  Knowledge Organisation Systems).
framework for thesauri. It  defines  skos   specific  expansion  of  the
hierarchy as well as editing the SKOS hierarchy.

Issues:
	- Menus for creating new individuals, etc.
	- Icons for skos class, part and individual.

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- rdf_register_ns(skos,  'http://www.w3.org/2004/02/skos/core#').
:- rdf_register_ns(skosm, 'http://www.w3.org/2004/02/skos/mapping#').

:- begin_rules(display, skos).

		 /*******************************
		 *	     HIERARCHY		*
		 *******************************/

%	child_cache(+Resource, -Cache, -Class)
%	
%	Used to expand the hierarchy. Resource is the resource expanded.
%	Cache is a mediator (see rdf_cache.pl)   that produces a list of
%	objects in the poper order to be   displayed below this node. It
%	is allowed to produce multiple  caches   for  a single node, for
%	example to show both sub-classes and individuals of a class. The
%	Class argument is the XPCE class used   to  create the childs of
%	this. We can use this to   define icons, drag-and-drop, etc. for
%	the child in this specific context.

child_cache(R, Cache, rdf_class_node) :-
	rdfs_individual_of(R, skos:'Concept'),
	rdf_cache(lsorted(V), skos_narrower(R, V), Cache).
child_cache(R, Cache, Class) :-
	super::child_cache(R, Cache, Class).

skos_narrower(Class, Narrow) :-
	rdf_has(Narrow, skos:broader, Class).
skos_narrower(Class, Narrow) :-
	rdf_has(Narrow, rdfs:subClassOf, Class).
skos_narrower(Class, Narrow) :-
	rdf_has(Class, skos:narrower, Narrow).

%	parent(+Resource, -Parent, -Class)
%	
%	Used to show the minimal tree that displays a given resource. We
%	could use child_cache/3 for this purpose,   but as this requires
%	generating the entire tree this is in general too slow.

parent(R, Parent, rdf_class_node) :-
	rdfs_individual_of(R, skos:'Concept'),
	skos_narrower(Parent, R).
parent(R, Parent, Role) :-
	super::parent(R, Parent, Role).


		 /*******************************
		 *	   DRAG AND DROP	*
		 *******************************/

%	drop_resource_command(+Onto, +Drop, -Command)
%	
%	Determine the command(s) to execute if an object representing Drop
%	is dropped onto an object representing Ondo.  

drop_resource_command(C, R, Command) :-
	rdfs_individual_of(C, skos:'Concept'),
	rdfs_individual_of(R, skos:'Concept'),
	skos_concept_relation(Command, _P).
drop_resource_command(C, R, Command) :-
	super::drop_resource_command(C, R, Command).

term_expansion(skos_concept_relation(X, Y0),
	       skos_concept_relation(X, Y)) :-
	rdf_global_term(Y0, Y).

skos_concept_relation(skos_narrower,    skos:broader).
skos_concept_relation(skos_instance_of, skos:broaderInstantive).
skos_concept_relation(skos_part_of,     skos:broaderPartitive).

%	drop_resource(+Command, +Onto, +Drop)
%	
%	Perform the rdf modifications for the   given Command if Drop is
%	dropped Onto.

drop_resource(Command, C, R) :-
	skos_concept_relation(Command, Property), !,
	rdf_set_object(R, Property, C).
drop_resource(Command, C, R) :-
	super::drop_resource(Command, C, R).

:- end_rules.
