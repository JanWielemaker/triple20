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
:- use_module(semweb(rdf_db)).
:- use_module(rdf_text).
:- use_module(rdf_label).
:- use_module(rdf_cache).


owl_description_attribute(X) :- rdf_equal(owl:oneOf, X).
owl_description_attribute(X) :- rdf_equal(owl:complementOf, X).
owl_description_attribute(X) :- rdf_equal(owl:unionOf, X).
owl_description_attribute(X) :- rdf_equal(owl:intersectionOf, X).

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

:- end_particle.

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
resource(wnclass,     image, image('16x16/wnclass.xpm')).

:- begin_particle(rdf_icon_rules, []).

icon(R, Icon) :-
	rdfs_individual_of(R, wns:'LexicalConcept'),
	new(Icon, image(resource(wnclass))).
icon(R, Icon) :-
	rdfs_individual_of(R, rdfs:'Class'), !,
	(   rdfs_individual_of(R, owl:'Restriction')
	->  ResName = restriction
	;   rdfs_subclass_of(R, rdfs:'Class')
	->  ResName = metaclass
	;   owl_description_attribute(Att),
	    rdf_has(R, Att, _)
	->  ResName = description
	;   ResName = class
	),
	new(Icon, image(resource(ResName))).
icon(R, Icon) :-
	rdfs_individual_of(R, rdf:'Property'), !,
	new(Icon, image(resource(property))).
icon(R, Icon) :-
	rdfs_individual_of(R, rdf:'List'),
	new(Icon, image(resource(list))).
icon(_, Icon) :-
	new(Icon, image(resource(individual))).

:- end_particle.


		 /*******************************
		 *	      HIERARCHY		*
		 *******************************/

:- begin_particle(class_hierarchy, []).

%	child_cache(+Resource, -Cache, -Class)
%	
%	Create a cache (see rdf_cache/3) for generating the childs of
%	Resource.  The child-nodes are created as instances of Class.

child_cache(R, Cache, Class) :-
	rdfs_individual_of(R, rdfs:'Class'),
	(   rdfs_subclass_of(R, rdf:'Property')
	->  (   rdf_cache(V, rdf_has(V, rdfs:subClassOf, R), Cache),
	        Class = rdf_class_node
	    ;   rdf_cache(V, root_property(R,V), Cache),
		Class = rdf_property_node
	    )
	;   (   rdf_cache(V, rdf_has(V, rdfs:subClassOf, R), Cache),
	        Class = rdf_class_node
	    ;   % \+ rdfs_subclass_of(R, rdfs:'Class'),
	        rdf_cache(V, rdf_has(V, rdf:type, R), Cache),
		Class = rdf_individual_node
	    ;	rdfs_subclass_of(R, owl:'Restriction'),
		rdf_cache(V, rdf_has(V, rdf:type, R), Cache),
		Class = owl_restriction_node	    
	    )
	).
child_cache(R, Cache, rdf_individual_node) :-
	rdfs_individual_of(R, rdf:'List'), !,
	rdf_cache(V, rdfs_member(V, R), Cache).


%	parent(+Resource, -Parent, -Class)
%	
%	Find parent relations to expand the hierarchy selectively for
%	showing Resource.

parent(R, Parent, rdf_class_node) :-
	rdf_has(R, rdfs:subClassOf, Parent), !.
parent(R, Parent, rdf_property_node) :-
	rdf_has(R, rdfs:subPropertyOf, Parent), !.
parent(R, Parent, rdf_individual_node) :-
	rdf_has(R, rdf:type, Parent), !.

%	root_property(+Class, -Property)
%	
%	Generate the instances of Class (a subclass of rdf:Property)
%	that have no super property.

root_property(Class, P) :-
	rdf_has(P, rdf:type, Class),
	\+ rdf_has(P, rdfs:subPropertyOf, _).

:- end_particle.


		 /*******************************
		 *	  BIND TO OBJECTS	*
		 *******************************/

:- begin_particle(display,
		  [ rdf_label_rules,
		    rdf_icon_rules,
		    class_hierarchy
		  ]).
:- end_particle.


:- begin_particle(rdf_individual_node, display).

%icon(_, Icon) :-
%	new(Icon, image(resource(individual))).

%child_cache(_, _, _) :- !, fail.

:- end_particle.

:- begin_particle(rdf_property_node, display).

child_cache(R, Cache, rdf_property_node) :-
	rdf_cache(V, rdf_has(V, rdfs:subPropertyOf, R), Cache).

:- end_particle.

