/*  File:    rdf_label.pl
    Author:  Jan Wielemaker
    Created: Jul 18 2003
    Purpose: Create labels
*/

:- module(rdf_label, []).
:- use_module(library(pce)).
:- use_module(rdf_text).
:- use_module(semweb(rdf_db)).
:- use_module(semweb(rdfs)).
:- use_module(semweb(rdf_edit)).
:- use_module(particle).


resource(class,       image, image('16x16/class.xpm')).
resource(metaclass,   image, image('16x16/Metaclass.gif')).
resource(orphanclass, image, image('16x16/orphanclass.xpm')).
resource(individual,  image, image('16x16/Instance.gif')).
resource(property,    image, image('16x16/SlotDirect.gif')).
resource(list,        image, image('16x16/list.xpm')).
resource(list_member, image, image('16x16/list_member.xpm')).
resource(untyped,     image, image('16x16/untyped.xpm')).
resource(resource,    image, image('16x16/resource.xpm')).
resource(restriction, image, image('16x16/restriction.xpm')).
resource(description, image, image('16x16/description.xpm')).


		 /*******************************
		 *	  COMPOSITE LABEL	*
		 *******************************/

:- pce_begin_class(rdf_composite_label, figure,
		   "Create labels from parts").
:- use_class_template(rdf_container).

variable(resource,  name,  get, "Represented resource").

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
	send(T, format, @rdf_composite_format).

print(T, Text:char_array) :->
	send(T, append, text(Text, @default, bold)).

append_resource(T, Value:prolog) :->
	"Append a resource value"::
	rdf_label_rules::label(Value, Label),
	send(T, append, Label).

:- pce_group(event).

event(T, Ev:event) :->
	(   send_super(T, event, Ev)
	->  true
	;   send(@rdf_resource_text_recogniser, event, Ev)
	).

entered(TF, Entered:bool) :->
	(   Entered == @on
	->  send(TF, pen, 1)
	;   send(TF, pen, 0)
	),
	(   Entered == @on,
	    send(TF, clipped_by_window)
	->  send(@unclip_window, attach, TF)
	;   true
	).

:- pce_group(test).

is_anonymous(TF) :->
	"Test if the object is unnamed"::
	get(TF, resource, Resource),
	(   rdf_has(Resource, rdfs:label, _)
	->  !, fail
	;   sub_atom(Resource, _, _, _, '__')
	).

:- pce_end_class(rdf_composite_label).


:- pce_begin_class(rdf_individual_label, rdf_composite_label,
		   "Typed individual").

update(L) :->
	get(L, resource, Resource),
	send(L, icon, resource(individual)),
	send(L, append, rdf_resource_text(Resource)).

:- pce_end_class(rdf_individual_label).

:- pce_begin_class(rdf_list_label, rdf_composite_label,
		   "Show elements of a list").

update(L) :->
	get(L, resource, List),
	send(L, print, '['),
	(   rdfs_member(Member, List),
	    send(L, append_resource, Member),
	    send(L, print, ', '),
	    fail
	;   send(L?graphicals?tail, string, ']')
	).

:- pce_end_class(rdf_list_label).


		 /*******************************
		 *	      RDFS		*
		 *******************************/

:- pce_begin_class(rdf_property_label, rdf_composite_label,
		   "Label for RDFS property declaration").

update(L) :->
	get(L, resource, Resource),
	send(L, icon, resource(property)),
	send(L, append, rdf_resource_text(Resource)).

:- pce_end_class(rdf_property_label).


:- pce_begin_class(rdfs_class_label, rdf_composite_label,
		   "Represent an RDFS class").

update(L) :->
	"Simple RDFS classes"::
	get(L, resource, Resource),
	send(L, icon, resource(class)),
	send(L, append, rdf_resource_text(Resource)).

:- pce_end_class(rdfs_class_label).


:- pce_begin_class(rdfs_metaclass_label, rdfs_class_label,
		   "Represent an RDFS class").

update(L) :->
	"Simple RDFS classes"::
	get(L, resource, Resource),
	send(L, icon, resource(metaclass)),
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

update(L) :->
	"OWL Specialised labels"::
	get(L, resource, Resource),
	send(L, icon, resource(description)),
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
	;   send(L, append, rdf_resource_text(Resource))
	).

:- pce_end_class(owl_description_label).


:- pce_begin_class(owl_restriction_label, rdf_composite_label,
		   "Represent an OWL restriction").

update(L) :->
	get(L, resource, Resource),
	send(L, icon, resource(restriction)),
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
