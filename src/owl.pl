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

:- module(owl,
	  [ owl_restriction_on/2,	% ?Class, ?Restriction
	    owl_merged_restriction/3,	% ?Class, ?Property, ?Restriction
	    owl_restriction/2,		% +Resource, -Restriction
	    owl_description/2,		% +Resource, -Description
	    owl_cardinality_on_subject/3, % +Subject, +Predicate, -Card
	    owl_cardinality_on_class/3,	% idem BJW
	    owl_satisfies/2,		% +Spec, +Resource
	    owl_individual_of/2,	% ?Resource, +Description

	    owl_direct_subclass_of/2,	% ?Resource, ?Class
	    owl_subclass_of/2,		% ?Class, ?Super

	    owl_has/3,			% ?Subject, ?Predicate, ?Object
	    owl_has_direct/3,		% ?Subject, ?Predicate, ?Object
	    owl_same_as/2,		% ?X, ?Y

	    owl_find/5			% +For, +Dom, ?Props, +Method, -Subj
	  ]).
:- use_module(library(lists)).
:- use_module(semweb(rdf_db)).
:- use_module(semweb(rdfs)).


		 /*******************************
		 *	    EXPANSION		*
		 *******************************/

%	user:goal_expansion(+NSGoal, -Goal)
%	
%	This predicate allows for writing down rdf queries in a friendly
%	name-space fashion.  

:- multifile
	user:goal_expansion/2.

user:goal_expansion(owl_restriction_on(Class0, restriction(Prop0, R)),
		    owl_restriction_on(Class, restriction(Prop, R))) :-
	rdf_global_id(Class0, Class),
	rdf_global_id(Prop0, Prop).
user:goal_expansion(owl_restriction(Res0, Restriction),
		    owl_restriction(Res, Restriction)) :-
	rdf_global_id(Res0, Res).
user:goal_expansion(owl_description(Res0, Description),
		    owl_description(Res, Description)) :-
	rdf_global_id(Res0, Res).
user:goal_expansion(owl_satisfies(Desc0, Res0),
		    owl_satisfies(Desc, Res)) :-
	rdf_global_term(Desc0, Desc),
	rdf_global_id(Res0, Res).
user:goal_expansion(owl_has(S0, P0, O0),
		    owl_has(S, P, O)) :-
	rdf_global_id(S0, S),
	rdf_global_id(P0, P),
	rdf_global_id(O0, O).
user:goal_expansion(owl_has_direct(S0, P0, O0),
		    owl_has_direct(S, P, O)) :-
	rdf_global_id(S0, S),
	rdf_global_id(P0, P),
	rdf_global_id(O0, O).
user:goal_expansion(owl_same_as(X0, Y0),
		    owl_same_as(X, Y)) :-
	rdf_global_id(X0, X),
	rdf_global_id(Y0, Y).

		 /*******************************
		 *	       FACTS		*
		 *******************************/

%	owl_individual(?IndividualID, ?Type)
%	owl_property(?IndividualID, ?PropertyID, ?PropertyValue)
%	owl_same_individual(?IndividualID1, ?IndividualID2)
%	owl_different_individual(?IndividualID1, ?IndividualID2)


		 /*******************************
		 *	      AXIOMS		*
		 *******************************/

%	owl_class(?ClassID, ?Super)
%	owl_class_modality(?ClassID, ?Modality)
%	owl_same_class(?ClassID1, ?ClassID2)


		 /*******************************
		 *	   RESTRICTIONS		*
		 *******************************/

%	owl_restriction(+ClassID, restriction(?PropertyID, ?Restriction))
%
%	Enumerate the restrictions that apply to PropertyID for Class.
%	Restriction is one of
%	
%		all_values_from(Class)
%		some_values_from(Class)
%		has_value(Value)
%		cardinality(Min, Max)

owl_restriction_on(Class, Restriction) :-
	owl_subclass_of(Class, RestrictionID),
	rdfs_individual_of(RestrictionID, owl:'Restriction'),
	owl_restriction(RestrictionID, Restriction).

owl_restriction(RestrictionID, restriction(Property, Restriction)) :-
	rdf_has(RestrictionID, owl:onProperty, Property),
	restriction_facet(RestrictionID, Restriction).

restriction_facet(RestrictionID, R) :-
	(   rdf_has(RestrictionID, owl:allValuesFrom, Class)
	->  R = all_values_from(Class)
	;   rdf_has(RestrictionID, owl:someValuesFrom, Class)
	->  R = some_values_from(Class)
	).
restriction_facet(RestrictionID, has_value(Value)) :-
	rdf_has(RestrictionID, owl:hasValue, Value).
restriction_facet(R, cardinality(Min, Max)) :-
	(   rdf_has(R, owl:cardinality, literal(Atom))
	->  non_negative_integer(Atom, Min, R, owl:cardinality),
	    Max = Min
	;   rdf_has(R, owl:minCardinality, literal(MinAtom))
	->  non_negative_integer(MinAtom, Min, R, owl:minCardinality),
	    (   rdf_has(R, owl:maxCardinality, literal(MaxAtom))
	    ->  non_negative_integer(MaxAtom, Max, R, owl:maxCardinality)
	    ;	Max = inf
	    )
	;   rdf_has(R, owl:maxCardinality, literal(MaxAtom))
	->  non_negative_integer(MaxAtom, Max, R, owl:maxCardinality),
	    Min = 0
	).
	
%	non_negative_integer(+Atom, -Integer, +Subject, +Predicate)
%	
%	Deduce integer value from rdf(Subject, Predicate, literal(Atom))
%	and if a conversion error occurs warn compatible to the rdfs_validate
%	library.
%	
%	TBD: If argument is typed we should check the type is compatible
%	to xsd:nonNegativeInteger.

non_negative_integer(type(_Type, Atom), Int, S, P) :-
	nonvar(Atom), !,
	non_negative_integer(Atom, Int, S, P).
non_negative_integer(Atom, Int, _, _) :-
	catch(atom_number(Atom, Int), _, fail), !,
	integer(Int),
	Int >= 0.
non_negative_integer(Atom, _, S, P) :-
	rdf_equal(xsd:nonNegativeInteger, Range),
	rdf_global_id(P, Pred),
	print_message(error,
		      rdf_illegal_object(S,Pred,literal(Atom),Range)),
	fail.

%	owl_merged_restriction(+Class, ?Property, ?Restriction)
%	
%	As owl_restriction_decl/3, but combines multiple restrictions
%	into the least strict restriction satisfying the declared
%	restrictions.

owl_merged_restriction(Class, Property, Restriction) :-
	setof(Decl,
	      owl_restriction(Class, restriction(Property, Decl)),
	      Decls),
	join_decls(Decls, Minimal),
	member(Restriction, Minimal).

%	input is sorted, thus the following holds:
%
%		cardinality < has_value < values_from

join_decls([], []).
join_decls([cardinality(Min1, Max1), cardinality(Min2, Max2)|T], Set) :- !,
	Min is max(Min1, Min2),
	max_cardinality(Max1, Max2, Max),
	join_decls([cardinality(Min, Max)|T], Set).
join_decls([has_value(Value)|T], [has_value(Value)]) :- !,
	satisfies_restrictions(T, Value).
join_decls([values_from(AS1, C1), values_from(AS2, C2)|T], Set) :-
	merge_values_from(AS1, C1, AS2, C2, AS, C), !,
	join_decls([values_from(AS, C)|T], Set).
join_decls([H|T0], [H|T]) :-
	join_decls(T0, T).

max_cardinality(infinite, Min, Min) :- !.
max_cardinality(Min, infinite, Min) :- !.
max_cardinality(Min1, Min2, Min) :-
	Min is min(Min1, Min2).
	
%	satisfies_restrictions(+Restrictions, +Value)
%
%	See whether Value satisfies all restrictions, so we can indeed
%	use it as a value.

satisfies_restrictions([], _).
satisfies_restrictions([H|T], Value) :-
	satisfies_restriction(H, Value),
	satisfies_restrictions(T, Value).

satisfies_restriction(has_value(Value), Value).
satisfies_restriction(values_from(some, _), _).
satisfies_restriction(values_from(all, Class), Value) :-
	rdfs_individual_of(Value, Class).

%	merge_values_from(+AllSome2, +C1, +AllSome2, +C2, -AllSome, -C)
%	
%	Merge multiple allValuesFrom and someValuesFrom restrictions.
%	This needs some thought, but as we don't need it for the MIA
%	tool right now we'll leave it.

merge_values_from(all, C1, all, C2, all, C) :-
	rdfs_subclass_of(C, C1),
	rdfs_subclass_of(C, C2).


		 /*******************************
		 *	    CARDINALITY		*
		 *******************************/

%	owl_cardinality_on_subject(+Subject, +Pred, -cardinality(Min, Max))
%	
%	Deduces the minimum and maximum cardinality for a property of a
%	resource.  This predicate may fail if no information is available.
%	
%	NOTE: used to use rdf_subclass_of.  Will owl_direct_subclass_of lead to
%	cycles?

owl_cardinality_on_subject(Subject, Predicate, Cardinality) :-
	findall(C, cardinality_on_subject(Subject, Predicate, C), L),
	join_decls(L, [Cardinality]).

cardinality_on_subject(Subject, Predicate, cardinality(Min, Max)) :-
	rdf_has(Subject, rdf:type, Class),
	owl_direct_subclass_of(Class, RestrictionID),
	rdfs_individual_of(RestrictionID, owl:'Restriction'),
	rdf_has(RestrictionID, owl:onProperty, Predicate),
	restriction_facet(RestrictionID, cardinality(Min, Max)).

owl_cardinality_on_class(Class, Predicate, Cardinality) :-
	findall(C, cardinality_on_class(Class, Predicate, C), L),
	join_decls(L, [Cardinality]).

cardinality_on_class(Class, Predicate, cardinality(Min, Max)) :-
	owl_direct_subclass_of(Class, RestrictionID),
	rdfs_individual_of(RestrictionID, owl:'Restriction'),
	rdf_has(RestrictionID, owl:onProperty, Predicate),
	restriction_facet(RestrictionID, cardinality(Min, Max)).




%	owl_satisfies_restriction(?Resource, +Restriction)
%	
%	True if Restriction satisfies the restriction imposed by Restriction.
%	The current implementation makes the following assumptions:
%	
%		# Only one of hasValue, allValuesFrom or someValuesFrom
%		  is present.

owl_satisfies_restriction(Resource, Restriction) :-
	rdf_has(Restriction, owl:onProperty, Property),
	(   rdf_has(Restriction, owl:hasValue, Value)
	->  owl_has(Resource, Property, Value)
	;   rdf_has(Restriction, owl:allValuesFrom, Class)
	->  setof(V, owl_has(Resource, Property, V), Vs),
	    all_individual_of(Vs, Class)
	;   rdf_has(Restriction, owl:someValuesFrom, Class)
	->  owl_has(Resource, Property, Value),
	    owl_individual_of(Value, Class)
	;   rdf_subject(Resource)
	),
	owl_satisfies_cardinality(Resource, Property, Restriction).

all_individual_of([], _).
all_individual_of([H|T], Class) :-
	owl_individual_of(H, Class), !,
	all_individual_of(T, Class).

%	owl_satisfies_cardinality(?Resource[, +Property], +Restriction)
%	
%	True if Resource satisfies the cardinality restrictions on
%	Property imposed by Restriction.

owl_satisfies_cardinality(Resource, Restriction) :-
	rdf_has(Restriction, owl:onProperty, Property),
	owl_satisfies_cardinality(Resource, Property, Restriction).

owl_satisfies_cardinality(Resource, Property, Restriction) :-
	rdf_has(Restriction, owl:cardinality, literal(Atom)), !,
	non_negative_int(Atom, Card),
	findall(V, rdf_has(Resource, Property, V), Vs0),
	sort(Vs0, Vs),			% remove duplicates
	length(Vs, Card).
owl_satisfies_cardinality(Resource, Property, Restriction) :-
	rdf_has(Restriction, owl:minCardinality, literal(MinAtom)),
	non_negative_int(MinAtom, Min), !,
	findall(V, owl_has(Resource, Property, V), Vs0),
	sort(Vs0, Vs),			% remove duplicates
	length(Vs, Count),
	Count >= Min,
	(   rdf_has(Restriction, owl:maxCardinality, literal(MaxAtom)),
	    atom_number(MaxAtom, Max)
	->  Count =< Max
	;   true
	).
owl_satisfies_cardinality(Resource, Property, Restriction) :-
	rdf_has(Restriction, owl:maxCardinality, literal(MaxAtom)),
	non_negative_int(MaxAtom, Max), !,
	findall(V, owl_has(Resource, Property, V), Vs0),
	sort(Vs0, Vs),			% remove duplicates
	length(Vs, Count),
	Count =< Max.
owl_satisfies_cardinality(Resource, _, _) :-
	rdf_subject(Resource).
	
non_negative_int(type(Type, Atom), Number) :-
	rdf_equal(xsd:nonNegativeInteger, Type),
	catch(atom_number(Atom, Number), _, fail).
non_negative_int(Atom, Number) :-
	atom(Atom),
	catch(atom_number(Atom, Number), _, fail).


		 /*******************************
		 *	    DESCRIPTION		*
		 *******************************/

%	owl_description(+DescriptionID, -Prolog)
%	
%	Convert an owl description into a Prolog representation.  This
%	representation is:
%	
%		class(Class)
%		restriction(Property, Restriction)
%		union_of(ListOfDescriptions)
%		intersection_of(ListOfDescriptions)
%		complement_of(Description)
%		one_of(Individuals)
%		
%	where Restriction is defined with owl_restriction/3.
%	
%	For example, the union-of can be the result of
%	
%	<rdfs:Class rdf:ID="myclass">
%	  <owl:unionOf parseType=Collection>
%	    <rdf:Description rdf:about="gnu"/>
%	    <rdf:Description rdf:about="gnat"/>
%	  </owl:unionOf>
%	</rdfs:Class>

owl_description(ID, Restriction) :-
	(   rdf_has(ID, rdf:type, owl:'Restriction')
	->  owl_restriction(ID, Restriction)
/*         owl_restriction(ID, restriction(P, F)), % unfolding problem BJW
	    F=.. [Facet, RClass],
	    owl_description(RClass, ClassDescription),
	    F1 =.. [Facet, ClassDescription],
	    Restriction = restriction(P, F1)
*/
	;   rdf_has(ID, rdf:type, owl:'Class') % was rdfs:'Class' BJW
	->  (   (   rdf_has(ID, owl:unionOf, Set)
		->  Restriction = union_of(SubDescriptions)
		;   rdf_has(ID, owl:intersectionOf, Set)
		->  Restriction = intersection_of(SubDescriptions)
		)
	    ->	rdfs_list_to_prolog_list(Set, Members),
		maplist(owl_description, Members, SubDescriptions)
	    ;	rdf_has(ID, owl:complementOf, Arg)
	    ->	Restriction = complement_of(SubDescription),
		owl_description(Arg, SubDescription)
	    ;	rdf_has(ID, owl:oneOf, Arg)
	    ->	Restriction = one_of(Individuals),
		rdfs_list_to_prolog_list(Arg, Individuals)
	    ;	Restriction = class(ID)
	    )
	).


		 /*******************************
		 *	   OWL_SATISFIES	*
		 *******************************/

%	owl_satisfies(+Specification, ?Resource)
%	
%	Test whether Resource satisfies Specification. All resources are
%	considered to belong  to  rdfs:Resource,   which  is  not really
%	enforced.
%	
%	Domain is one of
%	
%	  rdfs:Resource		  Allow for any resource
%	  class(Class)		  Allow for a subclass of Class
%	  union_of(Domains)	  
%	  intersection_of(Domains)
%	  complement_of(Domain)
%	  one_of(Resources)	  One of these values
%	  all_values_from(Class)  Individual of this class
%	  some_values_from(Class) Not used
%	  has_value(Value)	  Must have this value
%	
%	Resource can be a term individual_of(Class),  in which case this
%	predicate succeeds if any individual  of   Class  is accepted by
%	Domain.

					% Short-cut
owl_satisfies(Domain, Resource) :-
	rdf_equal(rdfs:'Resource', Domain), !,
	(   atom(Resource)
	->  true
	;   var(Resource)
	->  rdf_subject(Resource)
	;   Resource = individual_of(_)
	).
					% Descriptions
owl_satisfies(class(Domain), Resource) :- !,
	(   rdf_equal(Domain, rdfs:'Resource')
	->  true
	;   Resource = individual_of(Class),
	    atom(Class)
	->  fail
	;   owl_subclass_of(Resource, Domain)
	).
owl_satisfies(union_of(Domains), Resource) :- !,
	member(Domain, Domains),
	owl_satisfies(Domain, Resource).
owl_satisfies(intersection_of(Domains), Resource) :- !,
	in_all_domains(Domains, Resource).
owl_satisfies(complement_of(Domain), Resource) :- !,
	(   atom(Resource)
	->  true
	;   var(Resource)
	->  rdf_subject(Resource)
	;   fail			% individual_of(Class)
	),
	\+ owl_satisfies(Domain, Resource).
owl_satisfies(one_of(List), Resource) :- !,
	member(Resource, List).
					% Restrictions
owl_satisfies(all_values_from(Domain), Resource) :- !,
	(   Resource = individual_of(Class),
	    atom(Class)
	->  owl_subclass_of(Class, Domain)
	;   owl_individual_of(Resource, Domain)
	).
owl_satisfies(some_values_from(_Domain), _Resource) :- !.
owl_satisfies(has_value(Value), Resource) :-
	rdf_equal(Value, Resource).	% TBD: equality


in_all_domains([], _).
in_all_domains([H|T], Resource) :-
	owl_satisfies(H, Resource),
	in_all_domains(T, Resource).


		 /*******************************
		 *	   INDIVIDUAL OF	*
		 *******************************/

%	owl_individual_of(?Resource, +Description)
%	
%	Test or generate the resources that satisfy Description
%	according the the OWL-Description entailment rules.

owl_individual_of(Resource, Thing) :-
	rdf_equal(Thing, owl:'Thing'), !,
	(   atom(Resource)
	->  true
	;   rdf_subject(Resource)
	).
owl_individual_of(_Resource, Nothing) :-
	rdf_equal(Nothing, owl:'Nothing'), !,
	fail.
owl_individual_of(Resource, Description) :-
	rdfs_individual_of(Description, owl:'Class'),
	(   rdfs_individual_of(Description, owl:'Restriction')
	->  owl_satisfies_restriction(Resource, Description)
	;   rdf_has(Description, owl:unionOf, Set)
	->  rdfs_member(Sub, Set),
	    owl_individual_of(Resource, Sub)
	;   rdf_has(Description, owl:intersectionOf, Set)
	->  intersection_of(Set, Resource)
	;   rdf_has(Description, owl:complementOf, Arg)
	->  \+ owl_individual_of(Resource, Arg)
	;   rdf_has(Description, owl:oneOf, Arg)
	->  rdfs_member(Resource, Arg)
	;   rdfs_individual_of(Resource, Description)
	).
owl_individual_of(Resource, Description) :-
	rdfs_individual_of(Description, rdfs:'Class'),
	rdfs_individual_of(Resource, Description).
owl_individual_of(Resource, Description) :-
	owl_individual_from_range(Resource, Description).

owl_individual_from_range(Resource, Class) :-
	nonvar(Resource), !,
	rdf_has(_, P, Resource),
	rdf_has(P, rdfs:range, Class), !.
owl_individual_from_range(Resource, Class) :-
	rdf_has(P, rdfs:range, Class),
	rdf_has(_, P, Resource).	% owl_has?

intersection_of(List, Resource) :-
	rdf_has(List, rdf:first, First),
	owl_individual_of(Resource, First),
	(   rdf_has(List, rdf:rest, Rest)
	->  intersection_of(Rest, Resource)
	;   true
	).
intersection_of(Nil, _) :-
	rdf_equal(rdf:nil, Nil).


		 /*******************************
		 *	  OWL PROPERTIES	*
		 *******************************/

%	owl_has(?Subject, ?Predicate, ?Object)
%	
%	True if this relation is specified or can be deduced using OWL
%	inference ruls.

owl_has(S, P, O) :-
	(   var(P)
	->  rdfs_individual_of(P, rdf:'Property')
	;   true
	),
	rdf_reachable(SP, rdfs:subPropertyOf, P),
	owl_has_transitive(S, SP, O).


%	owl_has_transitive(?Subject, ?Predicate, ?Object)
%	
%	If Predicate is transitive, do a transitive closure on the
%	relation.

owl_has_transitive(S, P, O) :-
	rdfs_individual_of(P, owl:'TransitiveProperty'), !,
	owl_has_transitive(S, P, O, [P]).
owl_has_transitive(S, P, O) :-
	owl_has_equivalent(S, P, O).

owl_has_transitive(S, P, O, Visited) :-
	owl_has_equivalent(S, P, O1),
	O1 \= literal(_),
	\+ memberchk(O1, Visited),
	(   O = O1
	;   owl_has_transitive(O1, P, O, [O1|Visited])
	).

%	owl_has_equivalent(?Subject, ?Predicate, ?Object)
%	
%	Adds owl:sameAs on Subject and Object to owl_has_direct/3

owl_has_equivalent(S, P, O) :-
	nonvar(S), !,
	owl_same_as(S, S1),
	owl_has_direct(S1, P, O0),
	owl_same_as(O0, O).
owl_has_equivalent(S, P, O) :-
	nonvar(O), !,
	owl_same_as(O1, O),
	owl_has_direct(S0, P, O1),
	owl_same_as(S0, S).
owl_has_equivalent(S, P, O) :-
	owl_has_direct(S0, P, O0),
	owl_same_as(S0, S),
	owl_same_as(O0, O).


%	owl_same_as(?X, ?Y)
%	
%	True if X and Y are connected by the OWL identity relation.

owl_same_as(literal(X), literal(X)) :- !.
owl_same_as(X, Y) :-
	nonvar(X), !,
	owl_same_as(X, Y, [X]).
owl_same_as(X, Y) :-
	owl_same_as(Y, X, [X]).

owl_same_as(X, X, _).
owl_same_as(X, Y, Visited) :-
	(   rdf_has(X, owl:sameAs, X1)
	;   rdf_has(X1, owl:sameAs, X)
	),
	X1 \= literal(_),
	\+ memberchk(X1, Visited),
	owl_same_as(X1, Y, [X1|Visited]).


%	owl_has_direct(?Subject, ?Predicate, ?Object)
%	
%	Deals with `One-step' OWL inferencing: inverse properties,
%	symmetric properties and being subtype of a restriction with
%	an owl:hasValue statement on this property.

owl_has_direct(S, P, O) :-
	rdf(S, P, O).
owl_has_direct(S, P, O) :-
	(   rdf_has(P, owl:inverseOf, P2)
	->  true
	;   rdf_has(P2, owl:inverseOf, P)
	),
	rdf_has(O, P2, S).		% TBD: must call owl_has_direct/3
owl_has_direct(S, P, O) :-
	rdfs_individual_of(P, owl:'SymmetricProperty'),
	rdf(O, P, S).
owl_has_direct(S, P, O) :-
	owl_use_has_value(S, P, O).


%----------------------------------------------------------
% added by BJW for use of OWL with SWRL rules, highly experimental
% see http://www.daml.org/rules/proposal/rules-all.html for SWRL.
% It implements simple Prolog-like inferencing were order of antecedents
%  may matter and some assumptions about instantiation of variables are
%  made (see comments below).
% Currently is doesnot cater for arbitrary OWL descriptions mixed with
% SWRL.

owl_has_direct(S, P, O) :-
	owl_use_rule(S, P, O).

owl_use_rule(S, P, O):-
	rdf(Rule, rdf:type, swrl:'Impl'),     % pick a rule
	rdf(Rule, swrl:head, HeadList),
	rdfs_member(IPA, HeadList),           % can we use the rule?
	rdf(IPA, rdf:type, swrl:'IndividualPropertyAtom'),
	rdf(IPA, swrl:propertyPredicate, P),  % IndividualPropertyAtom
	rdf(Rule, swrl:body, BodyList),	      % yes
	rdfs_list_to_prolog_list(BodyList, BL),
	rdf_has(IPA, swrl:argument1, A1),
	rdf_has(IPA, swrl:argument2, A2),
	(   nonvar(S)
	->  (	nonvar(O) -> SL = [A1/S, A2/O]
	    ;	SL= [A1/S]
	    )
	;   nonvar(O)
	->  SL = [A2/O]
	;   SL = []
	),
	owl_evaluate_body(BL, SL, Subst),
	ignore(member(A1/S, Subst)), % make sure S and O are instantiated
	ignore(member(A2/O, Subst)). % could probably be done more elegantly
	
owl_evaluate_body([], Subst, Subst).
owl_evaluate_body([IPA| Rest], SL, Subst):-
	rdf(IPA, rdf:type, swrl:'IndividualPropertyAtom'),
	rdf(IPA, swrl:propertyPredicate, P), % IPA = IndividualPropertyAtom
	rdf_has(IPA, swrl:argument1, A1),    % maybe rdf instead of rdf_has? BJW
	rdf_has(IPA, swrl:argument2, A2),
	owl_has_swrl(A1, P, A2, SL, Subst1),
	owl_evaluate_body(Rest, Subst1, Subst).
owl_evaluate_body([DF| Rest], SL, Subst):-
	rdf(DF, rdf:type, swrl:'DifferentIndividualsAtom'),
	rdf_has(DF, swrl:argument1, A1),
	instantiated(A1, S, SL),	% assume both arguments are instantiated
	rdf_has(DF, swrl:argument2, A2),
	instantiated(A2, O, SL),	% this assumption is to be discussed
	\+ owl_same_as(S,O),
	owl_evaluate_body(Rest, SL, Subst).
owl_evaluate_body([SF| Rest], SL, Subst):-
	rdf(SF, rdf:type, swrl:'SameIndividualAtom'),
	rdf_has(SF, swrl:argument1, A1),
	instantiated(A1, S, SL),	% assume both arguments are instantiated
	rdf_has(SF, swrl:argument2, A2),
	instantiated(A2, O, SL),	% this assumption is to be discussed
	owl_same_as(S,O),		% 
	owl_evaluate_body(Rest, SL, Subst).
owl_evaluate_body([CA| Rest], SL, Subst):-
	rdf(CA, rdf:type, swrl:'ClassAtom'),
	rdf_has(CA, swrl:argument1, A1),
	(   instantiated(A1, S, SL) -> SL1=SL
	;   SL1 = [A1/S|SL]),
	rdf(CA, swrl:classPredicate, Class),
	owl_individual_of(S, Class),
	owl_evaluate_body(Rest, SL1, Subst).

owl_has_swrl(A1, P, A2, Subst, Subst):-	% this can probably be done better BJW
	instantiated(A1, S, Subst),
	instantiated(A2, O, Subst),!,	% dont backtrack here, proof complete
	owl_has(S, P, O).
owl_has_swrl(A1, P, A2, Subst, [A1/S|Subst]):-
	is_swrl_variable(A1),
	instantiated(A2, O, Subst),
	owl_has(S, P, O).
owl_has_swrl(A1, P, A2, Subst, [A2/O| Subst] ):-
	instantiated(A1, S, Subst),	
	is_swrl_variable(A2),
	owl_has(S, P, O).
owl_has_swrl(A1, P, A2, Subst, [A1/S, A2/O| Subst]):-  % too general?
	\+ instantiated(A1, S, Subst),
	\+ instantiated(A2, O, Subst),
	owl_has(S, P, O).

is_swrl_variable(V):-
	rdf_has(V, rdf:type, swrl:'Variable').

instantiated(A, A, _Subst):-
	\+ rdf_has(A, rdf:type, swrl:'Variable').
instantiated(A, S, Subst):-
	rdf_has(A, rdf:type, swrl:'Variable'),
	member(A/S, Subst).

%end additions BJW
%----------------------------------------------------------
owl_use_has_value(S, P, O) :-
	nonvar(P), !,
	rdf_has(Super, owl:onProperty, P),
	rdf_has(Super, owl:hasValue, O),
	owl_direct_subclass_of(Type, Super),
	rdf_has(S, rdf:type, Type).
owl_use_has_value(S, P, O) :-
	rdf_has(S, rdf:type, Type),
	owl_direct_subclass_of(Type, Super),
	rdfs_individual_of(Super, owl:'Restriction'),
	rdf_has(Super, owl:onProperty, P),
	rdf_has(Super, owl:hasValue, O).


		 /*******************************
		 *     OWL CLASS HIERARCHY	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
TBD: It is here that we must use a DL classifier!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%	owl_direct_subclass_of(-SubClass, +Class)
%	owl_direct_subclass_of(+SubClass, -Class)
%	
%	Returns both the RDFS subclasses and  classes that have Class in
%	their owl:intersectionOf attribute.
%	
%	The members of a union are its subclasses.

owl_direct_subclass_of(Class, R) :-
	rdf_has(Class, rdfs:subClassOf, R).
owl_direct_subclass_of(Class, R) :-	% added BJW (hack for symetry)
	rdf_has(R, owl:equivalentClass, Class).
owl_direct_subclass_of(Class, R) :-
	(   nonvar(R)
	->  (   rdf_has(R, owl:unionOf, Union),
	        rdfs_member(Class, Union)
	    ;   rdf_has(List, rdf:first, R),
		list_head(List, Head),
		rdf_has(Class, owl:intersectionOf, Head)
	    )
	;   nonvar(Class)
	->  (   rdf_has(Class, owl:intersectionOf, List),
	        rdfs_member(R, List)
	    ;   rdf_has(List, rdf:first, Class),
	        list_head(List, Head),
		rdf_has(R, owl:unionOf, Head)
	    )
	;   throw(error(instantiation_error, _))
	).

list_head(List, Head) :-
	(   rdf_has(H, rdf:rest, List)
	->  list_head(H, Head)
	;   Head = List
	).


%	owl_subclass_of(?Class, ?R)
%	
%	Recursive version of owl_subclass_of.

owl_subclass_of(Class, Super) :-
	rdf_equal(rdfs:'Resource', Resource),
	Super == Resource, !,
	(   nonvar(Class)
	->  true
	;   rdfs_individual_of(Class, owl:'Class')
	).
owl_subclass_of(Class, Super) :-
	nonvar(Class), !,
	owl_gen_supers(Class, [], Super).
owl_subclass_of(Class, Super) :-
	nonvar(Super), !,
	owl_gen_subs(Super, [], Class).
owl_subclass_of(_, _) :-
	throw(error(instantiation_error, _)).

owl_gen_supers(Class, _, Class).
owl_gen_supers(Class, Visited, Super) :-
	owl_direct_subclass_of(Class, Super0),
	\+ memberchk(Super0, Visited),
	owl_gen_supers(Super0, [Super0|Visited], Super).

owl_gen_subs(Class, _, Class).
owl_gen_subs(Class, Visited, Sub) :-
	owl_direct_subclass_of(Sub0, Class),
	\+ memberchk(Sub0, Class),
	owl_gen_subs(Sub0, [Sub0|Visited], Sub).
	

		 /*******************************
		 *     SEARCH IN HIERARCHY	*
		 *******************************/

%	owl_find(+String, +Domain, ?Properties, +Method, -Subject)
%	
%	Search all classes below Domain for a literal property with
%	that matches String.  Method is one of
%	
%		substring
%		word
%		prefix
%		exact
%		
%	domain is defined by owl_satisfy from owl.pl
%		
%	Note that the rdfs:label field is handled by rdfs_label/2,
%	making the URI-ref fragment name the last resort to determine
%	the label.

owl_find(String, Domain, Fields, Method, Subject) :-
	var(Fields), !,
	For =.. [Method,String],
	rdf_has(Subject, Field, literal(For, _)),
	owl_satisfies(Domain, Subject),
	Fields = [Field].		% report where we found it.
owl_find(String, Domain, Fields, Method, Subject) :-
	globalise_list(Fields, GlobalFields),
	For =.. [Method,String],
	member(Field, GlobalFields),
	(   Field == resource
	->  rdf_subject(Subject),
	    rdf_match_label(Method, String, Subject)
	;   rdf_has(Subject, Field, literal(For, _))
	),
	owl_satisfies(Domain, Subject).

globalise_list([], []) :- !.
globalise_list([H0|T0], [H|T]) :- !,
	globalise_list(H0, H),
	globalise_list(T0, T).
globalise_list(X, G) :-
	rdf_global_id(X, G).
