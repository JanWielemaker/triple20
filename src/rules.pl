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
label_class(Obj, ulan_timestamp_object_item) :-
	rdfs_individual_of(Obj, ulan:'TimeStamp').
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
:- use_module(semweb(rdfs)).

child(Resource, Role, Child, SubRole) :-
	rdfs_individual_of(Resource, rdfs:'Class'),
	send(class(Role), is_a, rdf_class_node),
	(   rdf_has(Child, rdfs:subClassOf, Resource),
	    (	rdfs_subclass_of(Child, rdfs:'Class')
	    ->	SubRole = rdf_metaclass_node
	    ;	rdfs_individual_of(Child, owl:'Restriction')
	    ->	SubRole = owl_restriction_node
	    ;	SubRole = rdf_class_node
	    )
	;   rdf_has(Child, rdf:type, Resource),
	    (	rdfs_individual_of(Child, rdf:'Property')
	    ->	SubRole = rdf_property_node
	    ;	rdfs_individual_of(Child, rdf:'List')
	    ->	\+ rdf_has(_, rdf:rest, Child),
	        SubRole = rdf_list_node
	    ;	SubRole = rdf_individual_node
	    )
	;   rdf_equal(Resource, rdfs:'Resource'),
	    orphan_class(_),
	    Child = '<Orphan Classes>',
	    SubRole = rdf_orphan_node
	).
child(Resource, Role, Child, Role) :-
	send(class(Role), is_a, rdf_property_node),
	rdf_has(Child, rdfs:subPropertyOf, Resource).
child(Resource, Role, Child, SubRole) :-
	send(class(Role), is_a, rdf_list_node),
	SubRole = rdf_list_member_node,
	rdfs_member(Child, Resource).
child('<Orphan Classes>', rdf_orphan_node, Orphan, SubRole) :-
	orphan_class(Orphan),
	(   rdfs_individual_of(Orphan, owl:'Restriction')
	->  SubRole = owl_restriction_node
	;   SubRole = rdf_class_node
	).
child('<Untyped Resources>', rdf_untyped_node, Orphan, rdf_resource_node) :-
	untyped_resource(Orphan).

orphan_class(Orphan) :-
	rdfs_individual_of(Orphan, rdfs:'Class'),
	\+ rdf_has(Orphan, rdfs:subClassOf, _),
	\+ rdf_equal(Orphan, rdfs:'Resource').

untyped_resource(S) :-
	rdf_subject(S),
	\+ rdf_has(S, rdf:type, _).


parent('<Orphan Classes>', Parent) :- !,
	rdf_equal(Parent, rdfs:'Resource').
parent('<Untyped Individuals>', Parent) :- !,
	rdf_equal(Parent, rdfs:'Resource').
parent(Resource, Parent) :-
	(   rdfs_individual_of(Resource, rdfs:'Class')
	->  (   rdf(Resource, rdfs:subClassOf, Parent)
	    ->  true
	    ;   Parent = '<Orphan Classes>'
	    )
	;   rdfs_individual_of(Resource, rdf:'Property'),
	    rdf_has(Resource, rdfs:subPropertyOf, Parent)
	->  true
	;   (   rdf(Resource, rdf:type, Parent)
	    ->	true
	    ;	Parent = '<Untyped Resources>'
	    )
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
