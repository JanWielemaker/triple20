/*  File:    ulan.pl
    Author:  Jan Wielemaker
    Created: Feb 27 2003
    Purpose: Show ulan-specific data
*/

:- module(ulan, []).
:- use_module(library(pce)).
:- use_module(semweb(rdf_db)).
:- use_module(semweb(rdfs)).
:- use_module(library(rdf_template)).

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
