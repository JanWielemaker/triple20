/*  $Id$

    Part of Triple20

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org/packages/Triple20
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

:- module(t20anonymous, []).
:- include(triple20(plugin)).
:- plugin([ rdfs:label   = 'Anonymous',
	    rdfs:comment = 'Display anonymous instances of classes using \c
		            the class label and a special icon'
	  ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This Triple20 plugin vizualises anonymous instances of classes using the
label of the class and an icon which   is a merged version of the normal
class and individual label.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(rdf_anon_label, rdf_individual_label,
		   "Display anonymous resource").
:- pce_end_class(rdf_anon_label).

:- begin_rules(display, anon).

label_class(Resource, rdf_anon_label) :-
	anon_individual(Resource, _), !.
label_class(Resource, Class) :-
	super::label_class(Resource, Class).

label_text(Resource, Text) :-
	anon_individual(Resource, Class), !,
	super::label_text(Class, Text).
label_text(Resource, Text) :-
	super::label_text(Resource, Text).

:- end_rules.

:- begin_rules(rdf_anon_label, anon).

resource(anon, image, image('16x16/anon.xpm')).

icon(Resource, Icon) :-
	anon_individual(Resource, _Class), !,
	new(Icon, image(resource(anon))).
icon(Resource, Icon) :-
	super::icon(Resource, Icon).

:- end_rules.

anon_individual(Resource, Class) :-
	rdf_is_bnode(Resource),
	rdf_equal(rdf:type, TypeP),
	rdf(Resource, TypeP, Class),
	\+ (  rdf(Resource, P, O),
	      (	  P \== TypeP
	      ;	  O \== Class
	      )
	   ).

