/*  $Id$

    Part of SWI-Prolog

    Author:        Bob Wielinga
    E-mail:        wielinga@swi.psy.uva.nl
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

:- module(owl_inference,
	  [ owl_property/4,
	    owl_property/3,
	    owl_satisfies_class/2,
	    property_from_class/3,
	    class_from_range/4,
	    owl_property_from_range/3,
	    infer_from_properties/2,
	    owl_class/1,
	    owl_individual/1,
	    owl_subject/1,
	    owl_individual_of/2,
	    owl_individual_unique_types/2,
	    owl_same_individual/2
	  ]).
:- use_module(semweb(rdf_db)).
:- use_module(sebweb(rdfs)).
:- use_module(owl).

:- multifile
	user:goal_expansion/2.

user:goal_expansion(owl_class(Subj0),
		    owl_class(Subj)) :-
	rdf_global_id(Subj0, Subj).
user:goal_expansion(owl_individual(Subj0),
		    owl_individual(Subj)) :-
	rdf_global_id(Subj0, Subj).
user:goal_expansion(owl_subject(Subj0),
		    owl_subject(Subj)) :-
	rdf_global_id(Subj0, Subj).
user:goal_expansion(owl_individual_of(Subj0, Pred0),
		    owl_individual_of(Subj, Pred)) :-
	rdf_global_id(Subj0, Subj),
	rdf_global_id(Pred0, Pred).
user:goal_expansion(owl_individual_unique_types(Subj0, Pred0),
		    owl_individual_unique_types(Subj, Pred)) :-
	rdf_global_id(Subj0, Subj),
	rdf_global_id(Pred0, Pred).
user:goal_expansion(owl_same_individual(Subj0, Pred0),
		    owl_same_individual(Subj, Pred)) :-
	rdf_global_id(Subj0, Subj),
	rdf_global_id(Pred0, Pred).
user:goal_expansion(owl_property(Subj0, Pred0, Obj0),
		    owl_property(Subj, Pred, Obj)) :-
	rdf_global_id(Subj0, Subj),
	rdf_global_id(Pred0, Pred),
	rdf_global_id(Obj0, Obj).
user:goal_expansion(owl_property_from_range(Subj0, Pred0, Obj0),
		    owl_property_from_range(Subj, Pred, Obj)) :-
	rdf_global_id(Subj0, Subj),
	rdf_global_id(Pred0, Pred),
	rdf_global_id(Obj0, Obj).
user:goal_expansion(owl_satisfies_class(Subj0, Pred0),
		    owl_satisfies_class(Subj, Pred)) :-
	rdf_global_id(Subj0, Subj),
	rdf_global_id(Pred0, Pred).
user:goal_expansion(class_from_range(Subj0, Pred0, Obj0, D0),
		    class_from_range(Subj, Pred, Obj, D)) :-
	rdf_global_id(Subj0, Subj),
	rdf_global_id(Pred0, Pred),
	rdf_global_id(Obj0, Obj),
	rdf_global_id(D0, D).
user:goal_expansion(property_from_class(Subj0, Pred0, Obj0),
		    property_from_class(Subj, Pred, Obj)) :-
	rdf_global_id(Subj0, Subj),
	rdf_global_id(Pred0, Pred),
	rdf_global_id(Obj0, Obj).
user:goal_expansion(infer_from_properties(Subj0, Pred0),
		    infer_from_properties(Subj, Pred)) :-
	rdf_global_id(Subj0, Subj),
	rdf_global_id(Pred0, Pred).


		 /*******************************
		 *	       BASICS		*
		 *******************************/

owl_class(Subject):-
	rdf(Subject, rdf:type, owl:'Class').

owl_individual(Subject):-
	owl_class(Class),
	rdf(S, rdf:type, Class),
	(   Subject=S; rdf(S, owl:sameAs, Subject)).

%    Should generate all resources that are subjects in an owl context. 
%         Curently it does't handle properties and anonymous subjects.

owl_subject(Subject):-			% could be made more efficient
	findall(C, owl_class(C), Classes),
	findall(I, owl_individual(I), Individuals),
	append(Classes, Individuals, AllSubjects),
	sort(AllSubjects, Subjects),
	member(Subject, Subjects).

		 /*******************************
		 *	       FACTS		*
		 *******************************/

%	owl_individual(?IndividualID, ?Type)
%	owl_property(?IndividualID, ?PropertyID, ?PropertyValue)
%	owl_same_individual(?IndividualID1, ?IndividualID2)
%	owl_different_individual(?IndividualID1, ?IndividualID2)

		 /*******************************
		 *      owl_individual		*
		 *******************************/
%	owl_individual(?IndividualID, ?Type)
%	generates owl individuals and their types
%       An OWL individual is defined as an instance of owl:Thing or
%       as a resource that is owl:sameAs another owl individual.
%	Thus classes from non-OWL ontologies can be made OWL
%	individuals. In the latter case its type is interpreted to be
%	its superclasses.
%	This predicate also generates all supertypes. I am not sure we
%	always want that.
%	Should (but doesnot) take equivalentClass into account, i.e. an
%	individual I that has type T, and equivalent(T, T1) holds, then
%	I is also of type T1.

owl_individual_of(IndividualID, Type):-
	owl_individual(IndividualID),
	owl_type(IndividualID, Type).

owl_type(GlobalIndividualID, Type):-
	rdfs_individual_of(GlobalIndividualID, Type).
owl_type(GlobalIndividualID, Type):-
	owl_same_individual(GlobalIndividualID, ID2), % it must be owl also
	rdfs_individual_of(ID2, Type).
owl_type(GlobalIndividualID, Type):-
	owl_same_individual(GlobalIndividualID, ID2), % it must be owl also
	rdfs_subclass_of(ID2, Type),	% it is also a class, find super
	Type \= ID2.
owl_type(GlobalIndividualID, Type):-    % it can be a class, find supers
	rdfs_subclass_of(GlobalIndividualID, Type),	
	Type \= GlobalIndividualID.	% ignore the identity

owl_individual_unique_types(IndividualID, Types):-
	findall(Type, owl_individual(IndividualID, Type), TL),
	list_to_set(TL, Types).

		 /*******************************
		 *      owl_same_individual		*
		 *******************************/

%	owl_same_individual(?IndividualID1, ?IndividualID2)
%	is symmetric, so sameAs(X,Y) implies sameAs(Y,X).
%	If both relations are defined in OWL, owl_same_individual
%	should not generate duplicates.
%	Currently not transitive.

owl_same_individual(IndividualID1, IndividualID2):-
	rdf_global_id(owl:sameAs, SameAs),
	rdf_global_id(IndividualID1, ID1),
	rdf_global_id(IndividualID2, ID2),
	rdf_has(ID1, SameAs, ID2),
	rdf_has(ID2, SameAs, ID1).
owl_same_individual(IndividualID1, IndividualID2):-
	rdf_global_id(owl:sameAs, SameAs),
	rdf_global_id(IndividualID1, ID1),
	rdf_global_id(IndividualID2, ID2),
	(   rdf_has(ID1, SameAs, ID2); rdf_has(ID2, SameAs, ID1)),
	\+ symmetric(ID1,ID2).


symmetric(ID1,ID2):-
	rdf_global_id(owl:sameAs, SameAs),
	rdf_has(ID1, SameAs, ID2),	% if not both are defined
	rdf_has(ID2, SameAs, ID1).
		
owl_property(Subject, Predicate, Object):-
	owl_property(Subject, Predicate, Object, _).


owl_property(Subject, Predicate, Object, TriplePred):-
	owl_property_recursive(Subject, Predicate, Object, TriplePred, []).
owl_property(Subject, Predicate, Object, TriplePred):-
	owl_property_transitive(Subject, Predicate, Object, TriplePred,[]).
owl_property(Subject, Predicate, Object, Predicate):-
	owl_property_from_range(Subject, Predicate, Object).			       

owl_property_recursive(Subject, Predicate, Object, _TriplePred, Visited):-
%format('testing predicate(~w, ~w,~w~nvisited ~w)', [Subject, Predicate, Object, Visited]),
	member(visited(Subject, Predicate, Object), Visited),!,
	fail.
owl_property_recursive(Subject, Predicate, Object, TriplePred, _Visited):-
%format('testing predicate(~w, ~w,~w~n)', [Subject, Predicate, Object]),
	rdf_has(Subject, Predicate, Object, TriplePred).
owl_property_recursive(Subject, Predicate, Object, TriplePred, Visited):-
	nonvar(Subject),
	rdf_has(Subject, owl:sameAs, NewSubject),
	owl_property_recursive(NewSubject, Predicate, Object, TriplePred,
			      [visited(Subject, Predicate, Object)|Visited]).
owl_property_recursive(Subject, Predicate, Object, TriplePred, Visited):-
	nonvar(Object),
	rdf_has(Object, owl:sameAs, NewObject), % maybe lift to owl_property?
	owl_property_recursive(Subject, Predicate, NewObject, TriplePred,
			      [visited(Subject, Predicate, Object)|Visited]).
owl_property_recursive(Subject, Predicate, Object, TriplePred, Visited):-
					% This may cause duplicate solutions,
					% since rdf_has already searched for
					% subproperties. However, 
					% this clause is needed in case
					% subproperties have special status
					% (such as inverseOf etc.) that the 
					% rdf(s) engine does not know about.
					% So, maybe the rdf_has solution is a
					% mixed blessing or should become owl_has?
	rdf_has(SubPredicate, rdfs:subPropertyOf, Predicate),
	owl_property_recursive(Subject, SubPredicate, Object, TriplePred,
			      [visited(Subject, Predicate, Object)|Visited]).
owl_property_recursive(Subject, Predicate, Object, TriplePred, Visited):-
	(   rdf_has(Predicate, owl:inverseOf, Inverse);
	    rdf_has(Inverse, owl:inverseOf, Predicate)),%may cause duplicates
	owl_property_recursive(Object, Inverse, Subject, TriplePred,
			      [visited(Subject, Predicate, Object)|Visited]).

owl_property_transitive(Subject, Predicate, Object, TriplePred, Visited):-
	rdf_has(Predicate, rdf:type, owl:'TransitiveProperty'),
	owl_property_recursive(Subject, Predicate, NewObject, _TriplePred,
			      Visited),
	owl_property_recursive(NewObject, Predicate, Object, TriplePred,
			      [visited(Subject, Predicate, NewObject)|Visited]).

owl_property_from_range(Object, Property, Value):-   % needs some thinking
	rdf(_S, P, Object, _Source), % where is it an object?
	rdf(P, rdfs:range, RangeClass),
	owl_restriction_on(RangeClass, restriction(Property, has_value(Value))).
%	extract_value(Constraint, ValueClass),
%	rdf(Value, rdfs:type, ValueClass). % find an instance

		 /*******************************
		 *experimental inference schemes*
		 *******************************/

owl_satisfies_class(Subject, Class):-	% should test all constraints
	owl_restriction_on(Class, restriction(Property, Constraint)),
	constraint_satisfied(Subject, Property, Constraint).

constraint_satisfied(Subject, Property, Constraint):-
	owl_property(Subject, Property, Value, _TriplePred),
	owl_satisfies(Constraint, Value).

class_from_range(Subject, Predicate, Object, Description):-
	owl_property(Subject, Predicate, Object, _TriplePred),
	rdf(Predicate, rdfs:range, Range),
	Description =.. [Range, Object].

	
property_from_class(Subject, 
		    ClassDescription, PropertyDescription):-
	rdf_global_id(Subject, Subject),
	ClassDescription =.. [Class, Subject],
	owl_restriction_on(Class, restriction(Property, Constraint)),
	extract_value(Constraint, Value),
	PropertyDescription =.. [has, Subject, Property, Value].
	
extract_value(has_value(Value), Value).	
extract_value(all_values_from(Value), Value).	

infer_from_properties(Subject, Descriptions):-
	findall(D, (owl_property(Subject, P, O),
		    owl_individual(O),
		    class_from_range(Subject, P, O, D)), Descriptions).
