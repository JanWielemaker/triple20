/*  File:    rules.pl
    Author:  Jan Wielemaker
    Created: Jun 25 2003
    Purpose: Define rendering and other rules
*/

:- module(rdf_rules,
	  [
	  ]).
:- use_module(particle).
:- use_module(semweb(rdfs)).
:- use_module(rdf_text).
:- use_module(rdf_label).
:- use_module(rdf_cache).


		 /*******************************
		 *	       LABELS		*
		 *******************************/

:- begin_particle(rdf_label_rules, []).

label(Resource, Label) :-
	::label_class(Resource, Class), !,
	Term =.. [Class, Resource],
	new(Label, Term).

%	label_class(+Resource, -Class) 
%	
%	Determine the visualiser to use for a short textual description
%	of a resource.  Resource is the resource for which to create a 
%	visualisation.  Role is one of subject, predicate or object and
%	Class is the XPCE class to use.

label_class(literal(_), rdf_literal_text) :- !.
label_class(Obj, ulan_timestamp_label) :-
	rdfs_individual_of(Obj, ulan:'TimeStamp').
label_class(Obj, wn_class_label) :-
	rdfs_individual_of(Obj, wns:'LexicalConcept').
label_class(Obj, rdf_property_label) :-
	rdfs_individual_of(Obj, rdf:'Property').
label_class(Obj, owl_restriction_label) :-
	rdfs_individual_of(Obj, owl:'Restriction').
label_class(Obj, LabelClass) :-
	rdfs_individual_of(Obj, owl:'Class'),
	(   owl_description_attribute(Att),
	    rdf_has(Obj, Att, _)
	->  LabelClass = owl_description_label
	;   LabelClass = owl_class_label
	).
label_class(Obj, LabelClass) :-
	rdfs_individual_of(Obj, rdfs:'Class'), !,
	(   rdfs_subclass_of(Obj, rdfs:'Class')
	->  LabelClass = rdfs_metaclass_label
	;   LabelClass = rdfs_class_label
	).
label_class(Obj, rdf_list_label) :-
	rdfs_individual_of(Obj, rdf:'List').
label_class(Obj, rdf_individual_label) :-
	rdf_has(Obj, rdf:type, _).
label_class(_, rdf_resource_text).

owl_description_attribute(X) :- rdf_equal(owl:oneOf, X).
owl_description_attribute(X) :- rdf_equal(owl:complementOf, X).
owl_description_attribute(X) :- rdf_equal(owl:unionOf, X).
owl_description_attribute(X) :- rdf_equal(owl:intersectionOf, X).


:- end_particle.


		 /*******************************
		 *	      HIERARCHY		*
		 *******************************/

:- begin_particle(class_hierarchy, []).

child_cache(R, Cache, Class) :-
	rdfs_individual_of(R, rdfs:'Class'),
	(   rdf_cache(V, rdf_has(V, rdfs:subClassOf, R), Cache),
	    Class = rdf_class_node
	;   rdf_cache(V, rdf_has(V, rdf:type, R), Cache),
	    Class = rdf_individual_node
	).

:- end_particle.


		 /*******************************
		 *	  BIND TO OBJECTS	*
		 *******************************/

:- begin_particle(display,
		  [ rdf_label_rules,
		    class_hierarchy
		  ]).
:- end_particle.
