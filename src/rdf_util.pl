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

:- module(rdf_util,
	  [ property_domain/3,		% +Subject, +Property, -Domain
	    property_type/3,		% +Subject, +Property, -Type
	    sort_by_label/2,		% +Resources, -Sorted
	    rdf_default_file/2,		% +Resources, -File
	    rdf_set_object/4,		% +S, +P, +O, +NewObject
	    rdf_set_object/3,		% +S, +P, +Object
	    rdf_add_object/3,		% +S, +P, +O
	    rdf_new_property/2,		% +S, +P
	    rdf_list_operation/3,	% +Action, +Triple, +Resource
	    rdf_delete_hierarchy/3	% +Root, +Relation, +Options
	  ]).
:- use_module(semweb(rdf_db)).
:- use_module(semweb(rdfs)).
:- use_module(semweb(rdf_edit)).
:- use_module(owl).
:- use_module(library(option)).

%	user:goal_expansion(+NSGoal, -Goal)
%	
%	This predicate allows for writing down rdf queries in a friendly
%	name-space fashion.  

:- multifile
	user:goal_expansion/2.

user:goal_expansion(rdf_set_object(Subj0, Pred0, Obj0),
		    rdf_set_object(Subj, Pred, Obj)) :-
	rdf_global_id(Subj0, Subj),
	rdf_global_id(Pred0, Pred),
	rdf_global_id(Obj0, Obj).
user:goal_expansion(rdf_set_object(Subj0, Pred0, Obj0, New0),
		    rdf_set_object(Subj, Pred, Obj, New)) :-
	rdf_global_id(Subj0, Subj),
	rdf_global_id(Pred0, Pred),
	rdf_global_id(Obj0, Obj),
	rdf_global_id(New0, New).
user:goal_expansion(rdf_add_object(Subj0, Pred0, Obj0),
		    rdf_add_object(Subj, Pred, Obj)) :-
	rdf_global_id(Subj0, Subj),
	rdf_global_id(Pred0, Pred),
	rdf_global_id(Obj0, Obj).
user:goal_expansion(rdf_delete_hierarchy(Root0, Pred0, Opts0),
		    rdf_delete_hierarchy(Root, Pred, Opts)) :-
	rdf_global_id(Root0, Root),
	rdf_global_id(Pred0, Pred),
	rdf_global_term(Opts0, Opts).


%	property_domain(+Subject, +Property, -Domain)
%	
%	Determine the domain of this property. Note that if the domain
%	is a class we want the selector to select a class by browsing
%	the class-hierarchy.  There is some issue around meta-classes
%	here.  Maybe we need class(Root, Meta)!

property_domain(Subject, Property, Domain) :-
	findall(R, property_restriction(Subject, Property, R), List),
	sort(List, Set),
	(   Set = [Domain]
	->  true
	;   Domain = intersection_of(Set)
	).

property_restriction(_, Property, R) :-
	rdf_has(Property, rdfs:range, Range),
	adjust_restriction(all_values_from(Range), R).
property_restriction(Subject, Property, R) :-
	rdf_has(Subject, rdf:type, Class),
	owl_restriction_on(Class, restriction(Property, R0)),
	adjust_restriction(R0, R).
	
adjust_restriction(cardinality(_,_), _) :- !,
	fail.
adjust_restriction(R, R).


%	property_type(+Subject, +Property, -Type)
%	
%	Classify the type of the object. For now the return values are
%	one of
%
%		# resource
%		Value is an arbitrary resource
%		# literal
%		Value is a literal
%		# list
%		Value is a an indivisual of rdf:List

property_type(Subject, Property, Type) :-
	property_domain(Subject, Property, Domain),
	(   Domain = all_values_from(LiteralClass),
	    rdfs_subclass_of(LiteralClass, rdfs:'Literal')
	->  Type = literal
	;   Domain = all_values_from(LiteralClass),
	    rdfs_subclass_of(LiteralClass, rdf:'List')
	->  Type = list
	;   Type = resource
	).

%	sort_by_label(+Resources, -Sorted)
%	
%	Sort a list of resources by `ns'-label.  Removes duplicates.
%	Note that objects can have multiple labels.  We will sort by
%	the first for the time being.  Maybe we should sort on the
%	first in alphabetical order.  I don't think we want alternative
%	ordering on backtracking.

sort_by_label(Resources, Sorted) :-
	tag_label(Resources, Tagged),
	keysort(Tagged, Sorted0),
	unique_unkey(Sorted0, Sorted).

tag_label([], []).
tag_label([H|T0], [K-H|T]) :-
	(   H = literal(K)
	->  true
	;   rdfs_ns_label(H, K)
	->  true
	),
	tag_label(T0, T).

unique_unkey([], []).
unique_unkey([H0|T0], [H|T]) :-
	remove_dups(H0, T0, T1),
	H0 = _Key-H,
	unique_unkey(T1, T).

remove_dups(H, [H|T0], T) :- !,
	remove_dups(H, T0, T).
remove_dups(P, [H|T0], [H|T]) :-	% Handle different resources with
	same_label(P, H), !,		% same label
	remove_dups(P, T0, T).
remove_dups(_, L, L).

same_label(L-_, L-_).


%	rdf_default_file(+Resource, -File)
%	
%	Where to store facts about Resource? Should be extended to
%	include triples (or at least relations).

rdf_default_file(Resource, File) :-
	rdf_has(Resource, rdf:type, Object, P),
	rdf(Resource, P, Object, File:_), !.
rdf_default_file(Resource, File) :-
	rdf(Resource, _, _, File:_), !.
rdf_default_file(_, user).


		 /*******************************
		 *	  EDIT OPERATIONS	*
		 *******************************/

%	rdf_set_object(+Subject, +Predicate, +Old, +New)
%	
%	Modify object aspect of a triple.   This code also checks checks
%	the domain, but does not yet check the cardinality.

rdf_set_object(Subject, Predicate, Old, New) :-
	property_domain(Subject, Predicate, Domain),
	(   owl_satisfies(Domain, New)
	->  rdfe_transaction(set_object(Subject, Predicate, Old, New),
			     modify_property_value)
	;   throw(error(domain_error(Domain, New), _))
	).

set_object(Subject, Predicate, '__not_filled', _New) :-
	rdf_default_file(Subject, File),
	rdfe_update(Subject, Predicate, '__not_filled', source(File)),
	fail.				% next clause
set_object(Subject, Predicate, Old, New) :-
	rdfe_update(Subject, Predicate, Old, object(New)).

%	rdf_set_object(+Subject, +Predicate, +New)
%	
%	Remove all rdf(Subject, Predicate, _) and add rdf(Subject,
%	Predicate, New).

rdf_set_object(Subject, Predicate, Object) :-
	property_domain(Subject, Predicate, Domain),
	(   owl_satisfies(Domain, New)
	->  rdfe_transaction(set_object(Subject, Predicate, Object),
			     modify_property_value)
	;   throw(error(domain_error(Domain, New), _))
	).

set_object(Subject, Predicate, Object) :-
	findall(O-P, rdf_has(Subject, Predicate, O), Pairs0),
	sort(Pairs0, Pairs),
	(   Pairs = []
	->  rdf_default_file(Subject, File),
	    rdfe_assert(Subject, Predicate, Object, File)
	;   Pairs = [Old-P|More]
	->  rdfe_update(Subject, P, Old, object(Object)),
	    (	member(OM-PM, More),
		rdfe_retractall(Subject, PM, OM),
		fail
	    ;	true
	    )
	).

%	rdf_add_object(Subject, Predicate, Object)
%	
%	Guarded adding of a new triple. Must validate cardinality
%	constraints too.

rdf_add_object(Subject, Predicate, Object) :-
	property_domain(Subject, Predicate, Domain),
	(   owl_satisfies(Domain, Object)
	->  rdfe_transaction(add_object(Subject, Predicate, Object),
			     add_property_value)
	;   throw(error(domain_error(Domain, Object), _))
	).

add_object(Subject, Predicate, Object) :-
	rdf_default_file(Subject, File),
	rdfe_assert(Subject, Predicate, Object, File).


%	rdf_new_property(+Subject, +Property)
%	
%	Add a dummy value for a new property on Subject that can be
%	filled by editing or drag-and-drop modification.
%
%	TBD: Check cardinality

rdf_new_property(Subject, Predicate) :-
	property_type(Subject, Predicate, Type),
	default_object(Type, Object, Source),
	(   var(Source)
	->  rdf_default_file(Subject, Source)
	;   true
	),
	rdfe_transaction(rdfe_assert(Subject, Predicate, Object, Source),
			 new_property).

default_object(resource, '__not_filled', user).
default_object(list, Nil, _) :-
	rdf_equal(Nil, rdf:nil).
default_object(literal, literal(''), _).


%	rdf_list_operation(+Action, +Triple, +Resource)
%	
%	If Triple is a triple whose object is rdf:nil or a proper RDF
%	list, merge Resource into this list according to Action:
%	
%		# append/prepend
%		Add Resource at the start/end of the list
%		
%		# delete
%		Remove resource from the list
%
%		# Modify
%		Replace the list by Resource (which must be a list)

rdf_list_operation(modify, rdf(S,P,O), New) :- !,
	(   rdfs_individual_of(New, rdf:'List')
	->  rdfe_transaction(set_object(S, P, O, New),
			     modify_property_value)
	;   rdf_equal(rdf:'List', ListClass),
	    throw(error(domain_error(all_values_from(ListClass), New), _))
	).
rdf_list_operation(append, rdf(S, P, O), New) :-
	tail_triple(S, P, O, Subject, Predicate, Object),
	rdfe_transaction(list_append(Subject, Predicate, Object, New),
			 append).
rdf_list_operation(prepend, rdf(S, P, O), New) :-
	rdfe_transaction(list_append(S, P, O, New),
			 prepend).
rdf_list_operation(delete, rdf(S, P, O), Resource) :-
	rdfe_transaction(list_delete(S, P, O, Resource), delete_from_list).


tail_triple(S, P, O, S, P, O) :-
	rdf_equal(O, rdf:nil), !.	% must use owl:sameIndividual
tail_triple(_, _, L, S, P, O) :-
	rdf_has(L, rdf:rest, O1, P1), !,
	tail_triple(L, P1, O1, S, P, O).
	

list_append(S, P, O, New) :-
	rdf_default_file(S, Source),
	rdf_node(Node),
	rdfe_assert(Node, rdf:type, rdf:'List', Source),
	rdfe_assert(Node, rdf:rest, O, Source),
	rdfe_assert(Node, rdf:first, New, Source),
	rdfe_update(S, P, O, object(Node)).

%	list_delete(+Subject, +Predicate, +List, +Resource)
%	
%	Delete Resource from List, which is connected to Subject using
%	Predicate. Actually, this is extremely tricky. We cannot delete
%	from a list while maintaining the identity of the list. We could
%	shift the rdf:first, but we still loose on a list with one
%	element that must be replaced by rdf:nil.

list_delete(S, P, O, Resource) :-
	rdf_has(O, rdf:first, Resource), !,
	rdf_has(O, rdf:rest, Rest),
	rdfe_update(S, P, O, object(Rest)),
	(   rdf(_, _, O)
	->  true
	;   rdfe_delete(O)			% TBD: too much?
	).
list_delete(_, _, O, Resource) :-
	rdf_has(O, rdf:rest, Rest, P1),
	list_delete(O, P1, Rest, Resource).
	
	
		 /*******************************
		 *	 HIERACHY DELETE	*
		 *******************************/

%	rdf_delete_hierarchy(+Root, +Property, +Options)
%	
%	Delete all objects reachable from Root through property that
%	have no relations of type property, unless they have another
%	relation to the root.  Options:
%	
%	    * unless_reachable_from(Root)
%	      Do not delete resources that can be reached from the
%	      given Root.

rdf_delete_hierarchy(Root, Property, Options) :-
	rdfe_transaction(delete_hierarchy(Root, Property, Options),
			 delete_hierarchy).

delete_hierarchy(Root, Property, Options) :-
	rdf_equal(rdfs:'Resource', RdfsResource),
	option(unless_reachable_from(Keep), Options, RdfsResource),
	rdfe_retractall(Root, Property, _),
	findall(X, rdf_reachable(X, Property, Root), Set0),
	findall(X, rdf_reachable(X, Property, Keep), KeepSet0),
	sort(Set0, Set1),
	sort(KeepSet0, KeepSet1),
	oset_diff(Set1, KeepSet1, DelSet),
	(   option(confirm(true), Options, false)
	->  length(DelSet, Len),
	    send(@display, confirm,
		 'Delete %d classes and all associated properties?',
		 Len)
	;   true
	),
	forall(member(R, DelSet),
	       rdfe_delete(R)).
	
	
