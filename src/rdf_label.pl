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


:- module(rdf_label, []).
:- use_module(library(pce)).
:- use_module(rdf_text).
:- use_module(semweb(rdf_db)).
:- use_module(semweb(rdfs)).
:- use_module(semweb(rdf_edit)).
:- use_module(particle).
:- use_module(rdf_template).

:- pce_autoload(ulan_timestamp_label,    ulan).

		 /*******************************
		 *	  COMPOSITE LABEL	*
		 *******************************/

:- pce_begin_class(rdf_composite_label, figure,
		   "Create labels from parts").
:- use_class_template(rdf_container).
:- use_class_template(rdf_resource_template).

variable(resource,  name,  get, "Represented resource").
variable(wrap,      {extend,wrap,wrap_fixed_width,clip}, get, "Wrapping mode").
variable(opaque,    bool, both, "Hide parts from event-handling").

class_variable(opaque, bool, @on).

initialise(T, Resource:name) :->
	send_super(T, initialise),
	send(T, slot, resource, Resource),
	send(T, update).

:- pce_group(build).

:- pce_global(@rdf_composite_format, make_rdf_composite_format).
make_rdf_composite_format(F) :-
	new(F, format(vertical, 1, @on)),
	send(F, row_sep, 2).

icon(T, Icon:image) :->
	"Set the (left-most) icon"::
	send(T, append, bitmap(Icon)),
	send(T, append, graphical(0,0,1,0)).

append(T, Gr:graphical) :->
	send(T, display, Gr),
	(   get(T, format, @nil)
	->  send(T, format, @rdf_composite_format)
	;   true
	).

print(T, Text:char_array) :->
	send(T, append, text(Text, @default, bold)).

append_resource(T, Value:prolog) :->
	"Append a resource value"::
	rdf_label_rules::label(Value, Label),
	send(T, append, Label).

:- pce_group(event).

event(T, Ev:event) :->
	(   get(T, opaque, @off),
	    send_super(T, event, Ev)
	->  true
	;   send(@rdf_resource_text_recogniser, event, Ev)
	).

arm(TF, Val:bool) :->
	"Preview activiity"::
	(   Val == @on
	->  send(TF, report, status, TF?resource),
	    (	send(TF, clipped_by_window),
		send(@grabbed_windows, empty)
	    ->  debug(arm, 'Arming ~p', [TF]),
	        send(@unclip_window, attach, TF)
	    ;	send(TF, pen, 1)
	    )
	;   send(TF, pen, 0),
	    send(TF, report, status, '')
	).

popup(P, Popup:popup) :<-
	call_rules(P, popup(P, Popup)).

:- pce_group(test).

is_anonymous(TF) :->
	"Test if the object is unnamed"::
	get(TF, resource, Resource),
	(   rdf_has(Resource, rdfs:label, _)
	->  !, fail
	;   sub_atom(Resource, _, _, _, '__')
	), !.

:- pce_group(layout).

margin(T, Width:int*, How:[{wrap,wrap_fixed_width,clip}]) :->
	"Wrap items to indicated width"::
	(   Width == @nil
	->  send(T, slot, wrap, extend),
	    send(T, format, @rdf_composite_format)
	;   send(T, slot, wrap, How),
	    How == wrap
	->  new(F, format(horizontal, Width, @off)),
	    send(F, column_sep, 2),
	    send(F, row_sep, 0),
	    send(T, format, F)
	;   tbd
	).

:- pce_end_class(rdf_composite_label).


:- pce_begin_class(rdf_individual_label, rdf_composite_label,
		   "Typed individual").

update(L) :->
	get(L, resource, Resource),
	call_rules(L, icon(Resource, Icon)),
	send(L, icon, Icon),
	send(L, append, rdf_resource_text(Resource)).

:- pce_end_class(rdf_individual_label).

:- pce_begin_class(rdf_not_filled_label, rdf_individual_label,
		   "Show __not_filled").

update(L) :->
	get(L, resource, Resource),
	call_rules(L, icon(Resource, Icon)),
	send(L, icon, Icon),
	send(L, append, new(T, text('<not filled>', left, italic))),
	send(T, colour, red).

:- pce_end_class(rdf_not_filled_label).


:- pce_begin_class(rdf_list_label, rdf_composite_label,
		   "Show elements of a list").

class_variable(opaque, bool, @off).

update(L) :->				% TBD: limit length
	get(L, resource, RDFList),
	rdfs_list_to_prolog_list(RDFList, List),
	(   List == []
	->  send(L, print, '[]')
	;   send(L, print, '['),
	    append_list(List, L),
	    send(L, print, ']')
	).

append_list([], _).
append_list([H], L) :- !,
	send(L, append_resource, H).
append_list([H|T], L) :-
	send(L, append_resource, H),
	send(L, print, ', '),
	append_list(T, L).

:- pce_end_class(rdf_list_label).


		 /*******************************
		 *	      RDFS		*
		 *******************************/

:- pce_begin_class(rdf_property_label, rdf_composite_label,
		   "Label for RDFS property declaration").

update(L) :->
	get(L, resource, Resource),
	call_rules(L, icon(Resource, Icon)),
	send(L, icon, Icon),
	send(L, append, rdf_resource_text(Resource)).

:- pce_end_class(rdf_property_label).


:- pce_begin_class(rdfs_class_label, rdf_composite_label,
		   "Represent an RDFS class").

update(L) :->
	"Simple RDFS classes"::
	get(L, resource, Resource),
	call_rules(L, icon(Resource, Icon)),
	send(L, icon, Icon),
	send(L, append, rdf_resource_text(Resource)).

:- pce_end_class(rdfs_class_label).


:- pce_begin_class(rdfs_metaclass_label, rdfs_class_label,
		   "Represent an RDFS class").

update(L) :->
	"Simple RDFS classes"::
	get(L, resource, Resource),
	call_rules(L, icon(Resource, Icon)),
	send(L, icon, Icon),
	send(L, append, rdf_resource_text(Resource)).

:- pce_end_class(rdfs_metaclass_label).


		 /*******************************
		 *	       OWL		*
		 *******************************/

:- pce_begin_class(owl_class_label, rdfs_class_label,
		   "Represent an OWL class").

:- pce_end_class(owl_class_label).


:- pce_begin_class(owl_description_label, owl_class_label,
		   "Represent an OWL class").

class_variable(opaque, bool, @off).

update(L) :->
	"OWL Specialised labels"::
	get(L, resource, Resource),
	call_rules(L, icon(Resource, Icon)),
	send(L, icon, Icon),
	(   send(L, is_anonymous)
	->  (   rdf_has(Resource, owl:oneOf, List)
	    ->  send(L, print, 'oneOf'),
		send(L, append_resource, List)
	    ;   rdf_has(Resource, owl:complementOf, Class)
	    ->  send(L, print, 'complementOf('),
		send(L, append_resource, Class),
		send(L, print, ')')
	    ;   rdf_has(Resource, owl:unionOf, List)
	    ->  send(L, print, 'unionOf'),
		send(L, append_resource, List)
	    ;   rdf_has(Resource, owl:intersectionOf, List)
	    ->  send(L, print, 'intersectionOf'),
		send(L, append_resource, List)
	    ;   send_super(L, update)
	    )
	;   send(L, append, rdf_resource_text(Resource)),
	    send(L, opaque, @on)
	).

:- pce_end_class(owl_description_label).


:- pce_begin_class(owl_restriction_label, rdf_composite_label,
		   "Represent an OWL restriction").

class_variable(opaque, bool, @off).

update(L) :->
	get(L, resource, Resource),
	call_rules(L, icon(Resource, Icon)),
	send(L, icon, Icon),
	rdf_has(Resource, owl:onProperty, Property),
	(   rdf_has(Resource, owl:cardinality, Card)
	->  send(L, append_resource, Property),
	    send(L, print, ': cardinality = '),
	    send(L, append_resource, Card)
	;   rdf_has(Resource, owl:maxCardinality, Card)
	->  send(L, append_resource, Property),
	    send(L, print, ': cardinality =< '),
	    send(L, append_resource, Card)
	;   rdf_has(Resource, owl:minCardinality, Card)
	->  send(L, append_resource, Property),
	    send(L, print, ': cardinality >= '),
	    send(L, append_resource, Card)
	;   rdf_has(Resource, owl:hasValue, Value)
	->  send(L, append_resource, Property),
	    send(L, print, '='),
	    send(L, append_resource, Value)
	;   rdf_has(Resource, owl:allValuesFrom, Value),
	    send(L, append_resource, Property),
	    send(L, print, ': allValuesFrom '),
	    send(L, append_resource, Value)
	;   rdf_has(Resource, owl:someValuesFrom, Value),
	    send(L, append_resource, Property),
	    send(L, print, ': someValuesFrom '),
	    send(L, append_resource, Value)
	;   send(L, display, rdf_resource_text(Resource))
	).

:- pce_end_class(owl_restriction_label).


:- pce_begin_class(wn_class_label, rdf_composite_label,
		   "Represent a WordNet class").

update(L) :->
	get(L, resource, Resource),
	call_rules(L, icon(Resource, Icon)),
	send(L, icon, Icon),
	(   rdf_has(Resource, wns:wordForm, Label),
	    send(L, append_resource, Label),
	    send(L, print, ', '),
	    fail
	;   send(L?graphicals?tail, free)
	).

:- pce_end_class(wn_class_label).
