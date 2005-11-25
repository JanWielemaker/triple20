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


:- module(t20ulan_timestamp, []).
:- include(triple20(plugin)).
:- plugin([ rdfs:label   = 'ULAN-timestamp',
	    rdfs:comment = 'Show old ULAN timestamp objects'
	  ]).

:- use_module(library(pce)).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).
:- use_module(triple20(rdf_rules)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This plugin deals with an old translation of the Getty ULAN vocabulary.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- begin_rules(display, ulan).

label_class(Obj, ulan_timestamp_label) :-
	rdfs_individual_of(Obj, ulan:'TimeStamp').
label_class(Obj, Class) :-
	super::label_class(Obj, Class).

:- end_rules.

:- pce_begin_class(ulan_timestamp_label, rdf_individual_label,
		   "Represent an ulan data").

variable(resource, name, get, "Represented resource").

update(D) :->
	get(D, resource, Resource),
	(   rdf_has(Resource, ulan:year, Year)
	->  (   rdfs_individual_of(Resource, ulan:'ExactYear')
	    ->  send(D, append_resource, Year)
	    ;   rdfs_individual_of(Resource, ulan:'ApproximateYear')
	    ->  send(D, print, 'ca. '),
		send(D, append_resource, Year)
	    ;   rdfs_individual_of(Resource, ulan:'Century')
	    ->  Year = literal(YearAtom),
		atom_number(YearAtom, YearInt),
		C is YearInt//100,
		send(D, print, string('%d-th century', C))
	    )
	;   send_super(D, update)
	).

:- pce_end_class(ulan_timestamp_label).
