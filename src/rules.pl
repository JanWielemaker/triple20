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


		 /*******************************
		 *	       LABELS		*
		 *******************************/

:- begin_particle(rdf_label_rules, []).

label(Resource, Label) :-
	::label_class(Resource, Class),
	Term =.. [Class, Resource],
	new(Label, Term).

%	label_class(+Resource, -Class) 
%	
%	Determine the visualiser to use for a short textual description
%	of a resource.  Resource is the resource for which to create a 
%	visualisation.  Role is one of subject, predicate or object and
%	Class is the XPCE class to use.

label_class(literal(_), rdf_literal_text) :- !.
label_class(Obj, rdf_resource_text) :-
	rdf_has(Obj, rdfs:label, _), !.
label_class(Obj, rdf_resource_text) :-	% anonymous node id
	\+ sub_atom(Obj, _, _, _, '__'), !.
label_class(Obj, ulan_timestamp_object_item) :-
	rdfs_individual_of(Obj, ulan:'TimeStamp').
label_class(Obj, owl_restriction_text) :-
	rdfs_individual_of(Obj, owl:'Restriction').
label_class(Obj, owl_class_text) :-
	rdfs_individual_of(Obj, owl:'Class').
label_class(Obj, rdf_list_label) :-
	rdfs_individual_of(Obj, rdf:'List').
label_class(_, rdf_resource_text).

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
		 *	      TABLES		*
		 *******************************/

:- begin_particle(rdf_table_rules, []).

object_visual(rdf(S,P,O), Table, ObjGraphical) :-
	::object_visual_class(rdf(S,P,O), Class),
	NewTerm =.. [Class, S, P, O, Table],
	new(ObjGraphical, NewTerm).

object_visual_class(rdf(_S,_P,literal(_O)), rdf_literal_text) :- !.
object_visual_class(rdf(_S,_P,O), rdf_object_list_browser) :-
	rdfs_individual_of(O, rdf:'List').
object_visual_class(rdf(_S,_P,O), ulan_timestamp_object_item) :-
	rdfs_individual_of(O, ulan:'TimeStamp').
object_visual_class(rdf(_S,_P,_O), rdf_object_text).

:- end_particle.

		 /*******************************
		 *	       LISTS		*
		 *******************************/

:- begin_particle(rdf_list_rules, []).

collection_item_class(rdf(_S, _P, literal(_)), _LB,
		      rdf_literal_text).
collection_item_class(_, _, rdf_object_text).

:- end_particle.



