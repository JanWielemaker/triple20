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
	    rdf_add_object/3		% +S, +P, +O
	  ]).
:- use_module(semweb(rdf_db)).
:- use_module(semweb(rdfs)).
:- use_module(semweb(rdf_edit)).
:- use_module(owl).

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
%	one of `resource' and `literal'.  May be extended in the future.

property_type(Subject, Property, Type) :-
	property_domain(Subject, Property, Domain),
	(   Domain = all_values_from(LiteralClass),
	    rdfs_subclass_of(LiteralClass, rdfs:'Literal')
	->  Type = literal
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


%	rdf_add_object(Subject, Predicate, Object)
%	
%	Guarded adding of a new triple. Must validate cardinality
%	constraints too.

rdf_add_object(Subject, Predicate, Object) :-
	property_domain(Subject, Predicate, Domain),
	(   owl_satisfies(Domain, Object)
	->  rdfe_transaction(add_object(Subject, Predicate, Object),
			     modify_property_value)
	;   throw(error(domain_error(Domain, Object), _))
	).

add_object(Subject, Predicate, Object) :-
	rdf_default_file(Subject, File),
	rdfe_transaction(rdfe_assert(Subject, Predicate, Object, File),
			 add_property).
