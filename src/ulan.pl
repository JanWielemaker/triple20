/*  File:    ulan.pl
    Author:  Jan Wielemaker
    Created: Feb 27 2003
    Purpose: Show ulan-specific data
*/

:- module(ulan, []).
:- use_module(library(pce)).
:- use_module(library(rdf_db)).
:- use_module(library(rdfs)).
:- use_module(library(rdf_template)).

:- pce_begin_class(ulan_timestamp_item, device,
		   "Represent an ulan data").
:- use_class_template(rdf_container).

variable(resource, name, get, "Represented resource").

initialise(D, Resource:name) :->
	"Create from resource"::
	send_super(D, initialise),
	send(D, format, format(horizontal, 1, @on)),
	send(D, resource, Resource).

resource(D, Resource:name) :->
	send(D, slot, resource, Resource),
	send(D, clear),
	rdf_equal(ulan:year, YearPred),
	rdf_has(Resource, ulan:year, literal(YearAtom)),
	atom_number(YearAtom, Year),
	(   rdfs_individual_of(Resource, ulan:'ExactYear')
	->  send(D, display,
		 rdf_literal_text(Resource, YearPred, Year, D))
	;   rdfs_individual_of(Resource, ulan:'ApproximateYear')
	->  send(D, display,
		 rdf_literal_text(Resource, YearPred, Year, D))
	;   rdfs_individual_of(Resource, ulan:'Century')
	->  C is Year//100,
	    send(D, display, text(string('%d-th century', C)))
	).

:- pce_end_class(ulan_timestamp_item).


		 /*******************************
		 *        SHOW OBJECT FIELD	*
		 *******************************/

:- pce_begin_class(ulan_timestamp_object_item, ulan_timestamp_item,
		   "Represent ulan timestamp as object-part").

variable(subject,   name*, get, "RDF subject").
variable(predicate, name*, get, "RDF predicate").

initialise(T,
	   Subject:subject=name,
	   Predicate:predicate=name,
	   Value:object=name,		% collection resource
	   _Container:container=[object]) :->
	send(T, slot, subject, Subject),
	send(T, slot, predicate, Predicate),
	send_super(T, initialise, Value).

:- pce_end_class(ulan_timestamp_object_item).
