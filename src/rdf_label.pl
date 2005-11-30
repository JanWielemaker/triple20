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
:- use_module(rdf_rules).
:- use_module(particle).
:- use_module(rdf_template).
:- use_module(rdf_cache).
:- use_module(rdf_util).
:- use_module(library(debug)).


		 /*******************************
		 *	  COMPOSITE LABEL	*
		 *******************************/

:- pce_begin_class(rdf_composite_label, figure,
		   "Create labels from parts").
:- use_class_template(rdf_container).
:- use_class_template(rdf_resource_template).

variable(resource,  name,  get, "Represented resource").
variable(wrap,      {extend,wrap,wrap_fixed_width,clip} := extend, get,
	 "Wrapping mode").
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
	"Append an icon"::
	send(T, append, bitmap(Icon)).

prefix_icon(T, Icon:image) :->
	"Prepend (left-most) icon"::
	send(T, append, new(BM, bitmap(Icon))),
	send(BM, hide).			% first in <-graphicals

append(T, Gr:graphical) :->
	(   send(T?graphicals?tail, instance_of, bitmap),
	    \+ send(Gr, instance_of, bitmap)
	->  send(T, display, graphical(0,0,1,0))
	;   true
	),
	send(T, display, Gr),
	(   get(T, format, @nil)
	->  send(T, format, @rdf_composite_format)
	;   true
	).

print(T, Text:char_array) :->
	send(T, append, text(Text, @default, bold)).

append_resource(T, Value:prolog) :->
	"Append a resource value"::
	call_rules(T, label(Value, Label)),
	send(T, append, Label).
append_resource(T, Value:prolog, Label:graphical) :<-
	"Append a resource value"::
	call_rules(T, label(Value, Label)),
	send(T, append, Label).

display_icons(T) :->
	"Display (append) relevant icons"::
	get(T, resource, Resource),
	(   call_rules(T, icon(Resource, Icon)),
	    send(T, icon, Icon),
	    fail
	;   true
	).

display_resource(T) :->
	"Display resource text for <-resource"::
	get(T, resource, Resource),
	send(T, append, rdf_resource_text(Resource)).

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

:- pce_group(drap_and_drop).

prolog_source(P, Source:name) :<-
	"Allows dropping objects on PceEmacs"::
	get(P, resource, R),
	(   rdf_global_id(NS:Local, R)
	->  sformat(Source, '~q', [NS:Local])
	;   sformat(Source, '~q', [R])
	).


:- pce_group(test).

is_anonymous(TF) :->
	"Test if the object is unnamed"::
	get(TF, resource, Resource),
	\+ rdf_has(Resource, rdfs:label, _),
	sub_atom(Resource, _, _, _, '__'), !.

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


:- pce_begin_class(rdf_icon_label, rdf_composite_label,
		   "Display as icon with text").

initialise(L, R:name) :->
	send_super(L, initialise, R),
	send(@resource_texts, append, R, L).

unlink(L) :->
	get(L, resource, R),
	send(@resource_texts, delete, R, L),
	send_super(L, unlink).

update(L) :->
	send(L, clear, destroy),
	send(L, display_icons),
	send(L, display_resource).

:- pce_end_class(rdf_icon_label).


:- pce_begin_class(rdf_resource_label, rdf_icon_label,
		   "Untyped resource").
:- pce_end_class(rdf_resource_label).


:- pce_begin_class(rdf_individual_label, rdf_icon_label,
		   "Typed individual").
:- pce_end_class(rdf_individual_label).


:- pce_begin_class(rdf_property_label, rdf_icon_label,
		   "Label for RDFS property declaration").

:- pce_end_class(rdf_property_label).


:- pce_begin_class(rdfs_class_label, rdf_icon_label,
		   "Represent an RDFS class").

:- pce_end_class(rdfs_class_label).


:- pce_begin_class(rdfs_metaclass_label, rdf_icon_label,
		   "Represent an RDFS class").

:- pce_end_class(rdfs_metaclass_label).


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

variable(cache,	   int*, get, "Cache for members").
variable(max_size, int,  get, "Maximum elements to show").

class_variable(max_size, int, 5).
class_variable(opaque, bool, @off).

initialise(T, Resource:name) :->
	(   rdf_equal(Resource, rdf:nil)
	->  true
	;   rdf_cache(V, rdfs_member(V, Resource), Cache),
	    send(T, slot, cache, Cache),
	    rdf_cache_attach(Cache, T)
	),
	send_super(T, initialise, Resource).


unlink(T) :->
	(   get(T, cache, Cache),
	    Cache \== @nil
	->  rdf_cache_detach(Cache, T)
	;   true
	),
	send_super(T, unlink).

update(L, _Cache:[int]) :->
	get(L, cache, Cache),
	send(L, clear, destroy),
	(   Cache == @nil		% rdf:nil
	->  send(L, print, '[]')
	;   get(L, max_size, Max),
	    rdf_cache_cardinality(Cache, Size),
	    (	Size > Max
	    ->	send(L, print, '['),
		rdf_cache_result(Cache, Index, Value),
		send(L, append_resource, Value),
		(   Index < Max
		->  send(L, print, ', '),
		    fail
		;   !,
		    send(L, print, ', ..., '),
		    rdf_cache_result(Cache, Size, Last),
		    send(L, append_resource, Last),
		    send(L, print, ']')
		)
	    ;	send(L, print, '['),
		(   rdf_cache_result(Cache, Index, Value),
		    send(L, append_resource, Value),
		    (	Index < Size
		    ->	send(L, print, ', ')
		    ;	true
		    ),
		    fail
		;   send(L, print, ']')
		)
	    )
	).

delete_member(L, Part:graphical) :->
	"Called from the delete menu on parts"::
	send(Part, has_get_method, resource),
	get(Part, resource, Resource),
	get(L, triple, Triple),
	rdf_list_operation(delete, Triple, Resource).

:- pce_end_class(rdf_list_label).


		 /*******************************
		 *	       OWL		*
		 *******************************/

:- pce_begin_class(owl_class_label, rdfs_class_label,
		   "Represent an OWL class").

:- pce_end_class(owl_class_label).


:- pce_begin_class(owl_description_label, owl_class_label,
		   "Represent an OWL class").

variable(predicate, name*, get, "Represented predicate").
class_variable(opaque, bool, @off).

initialise(L, R:name) :->
	send_super(L, initialise, R),
	send(@resource_texts, append, R, L).

unlink(L) :->
	get(L, resource, R),
	send(@resource_texts, delete, R, L),
	send_super(L, unlink).

update(L) :->
	"OWL Specialised labels"::
	send(L, clear, destroy),
	send(L, slot, predicate, @nil),
	get(L, resource, Resource),
	call_rules(L, icon(Resource, Icon)),
	send(L, icon, Icon),
	(   send(L, is_anonymous)
	->  (   rdf_has(Resource, owl:oneOf, List, P)
	    ->  send(L, print, 'oneOf'),
		send(L, append_resource, List)
	    ;   rdf_has(Resource, owl:complementOf, Class, P)
	    ->  send(L, print, 'complementOf('),
		send(L, append_resource, Class),
		send(L, print, ')')
	    ;   rdf_has(Resource, owl:unionOf, List, P)
	    ->  send(L, print, 'unionOf'),
		send(L, append_resource, List)
	    ;   rdf_has(Resource, owl:intersectionOf, List, P)
	    ->  send(L, print, 'intersectionOf'),
		send(L, append_resource, List)
	    ;   send_super(L, update)
	    ),
	    (	atom(P)
	    ->	send(L, slot, predicate, P)
	    ;	true
	    )
	;   send(L, append, rdf_resource_text(Resource)),
	    send(L, opaque, @on)
	).

:- pce_group(edit).

triple_from_part(L, Part:graphical, Triple:prolog) :<-
	"Find the triple representing the description"::
	send(@pce, write_ln, 'Triple from:', L, Part),
	get(L, predicate, Predicate),
	Predicate \== @nil,
	get(Part, container, @arg1?device == L, Member),
	send(Member, has_get_method, resource),
	get(Member, resource, Object),
	get(L, resource, Subject),
	Triple = rdf(Subject, Predicate, Object).

%	->owl_description_type: Type=name
%	
%	Set the type of the OWL description.  Type is the full owl
%	predicate name for the attribute.

owl_description_type(L, Type:name) :->
	rdfe_transaction(owl_description_type(L, Type),
			 owl_description_type(Type)).

owl_description_type(L, Type) :-
	get(L, resource, Subject),
	get(L, predicate, Predicate),
	(   Predicate == Type
	->  send(L, report, status, 'No change')
	;   Predicate \== @nil,
	    rdf(Subject, Predicate, Object)
	->  rdfe_update(Subject, Predicate, Object, predicate(Type)),
	    (	rdf_has(Type, rdfs:range, rdf:'List')
	    ->	(   rdfs_individual_of(Object, rdf:'List')
		->  true
		;   rdf_equal(rdf:nil, Nil),
		    rdfe_update(Subject, Type, Object, object(Nil))
		)
	    ;	(   rdfs_individual_of(Object, rdf:'List')
		->  rdfe_update(Subject, Type, Object, object('__not_filled'))
		;   true
		)
	    )
	;   rdf_new_property(Subject, Type)
	).


:- pce_end_class(owl_description_label).


:- pce_begin_class(owl_restriction_label, rdf_composite_label,
		   "Represent an OWL restriction").

class_variable(opaque, bool, @off).

update(L) :->
	get(L, resource, Resource),
	send(L, display_icons),
	(   send(L, is_anonymous)
	->  (   rdf_has(Resource, owl:onProperty, Property)
	    ->  send(L, append_resource, Property)
	    ;   send(L, print, '??')
	    ),
	    (   rdf_has(Resource, owl:cardinality, Card)
	    ->  send(L, print, ': cardinality = '),
		send(L, append_resource, Card)
	    ;   rdf_has(Resource, owl:maxCardinality, Card)
	    ->  send(L, print, ': cardinality =< '),
		send(L, append_resource, Card)
	    ;   rdf_has(Resource, owl:minCardinality, Card)
	    ->  send(L, print, ': cardinality >= '),
		send(L, append_resource, Card)
	    ;   rdf_has(Resource, owl:hasValue, Value)
	    ->  send(L, print, '='),
		send(L, append_resource, Value)
	    ;   rdf_has(Resource, owl:allValuesFrom, Value)
	    ->  send(L, print, ': allValuesFrom '),
		send(L, append_resource, Value)
	    ;   rdf_has(Resource, owl:someValuesFrom, Value)
	    ->  send(L, print, ': someValuesFrom '),
		send(L, append_resource, Value)
	    ;   true
	    )
	;   send(L, display_resource)
	).

:- pce_end_class(owl_restriction_label).


:- pce_begin_class(wn_class_label, rdf_icon_label,
		   "Represent a WordNet class").

display_resource(L) :->
	get(L, resource, Resource),
	(   (   rdf_has(Resource, wns:wordForm, Label)
	    *->	send(L, append_resource, Label),
		send(L, print, ', '),
		fail
	    ;	send_super(L, display_resource)
	    )
	;   send(L?graphicals?tail, free)
	).

:- pce_end_class(wn_class_label).
