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
	    rdf_default_file/3,		% +Resources, -File, -NS
	    rdf_default_ns/2,		% +File, -Namespace
	    rdf_set_default_ns/2,	% +File, +Namespace

	    rdf_set_dialect/1,		% Set the RDF dialect
	    rdf_current_dialect/1,	% Query the RDF dialect

					% Edit operations
	    rdf_set_object/4,		% +S, +P, +O, +NewObject
	    rdf_set_object/3,		% +S, +P, +Object
	    rdf_set_rev_object/4,	% +S, +P, +Rev, +Object
	    rdf_add_object/3,		% +S, +P, +O
	    rdf_new_property/2,		% +S, +P
	    rdf_new_property/3,		% +S, +P, +O
	    rdf_list_operation/3,	% +Action, +Triple, +Resource
	    rdf_delete_hierarchy/3,	% +Root, +Relation, +Options

	    rdf_merge_files/2,		% +Into, +From
	    
	    rdf_change_resource/2	% +From, +To
	  ]).
:- use_module(semweb(rdf_db)).
:- use_module(semweb(rdfs)).
:- use_module(semweb(rdf_edit)).
:- use_module(owl).
:- use_module(library(option)).
:- use_module(library(lists)).
:- use_module(rdf_rules).

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
user:goal_expansion(rdf_set_rev_object(Subj0, Pred0, Rev0, Obj0),
		    rdf_set_rev_object(Subj, Pred, Rev, Obj)) :-
	rdf_global_id(Subj0, Subj),
	rdf_global_id(Pred0, Pred),
	rdf_global_id(Rev0, Rev),
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


		 /*******************************
		 *     DIALECT SELECTION	*
		 *******************************/

:- dynamic
	dialect/1.

dialect(owl_full).			% initial default

%	rdf_set_dialect(+Dialect)
%	rdf_current_dialect(?Dialect).
%	
%	Set/query the current dialect. Allowed values are rdfs,
%	owl_lite, owl_dl or owl_full.

rdf_set_dialect(Dialect) :-
	(   dialect(Dialect)
	->  true
	;   retractall(dialect(_)),
	    assert(dialect(Dialect)),
	    broadcast(rdf_dialect(Dialect))
	).

rdf_current_dialect(Dialect) :-
	(   dialect(Dialect)
	->  true
	;   Dialect == owl
	->  \+ dialect(rdfs)
	).


		 /*******************************
		 *	PROPERTY HANDLING	*
		 *******************************/


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
%		# literal(Type)
%		Value is a literal
%		# list
%		Value is a an indivisual of rdf:List

property_type(Subject, Property, Type) :-
	property_domain(Subject, Property, Domain),
	(   Domain = all_values_from(Class)
	->  (   rdfs_subclass_of(Class, rdfs:'Literal')
	    ->  Type = literal(atom)
	    ;   rdfs_subclass_of(Class, xsd:nonNegativeInteger)
	    ->	Type = literal(integer(non_negative))
	    ;   rdfs_subclass_of(Class, rdf:'List')
	    ->  Type = list
	    ;	Type = resource
	    )
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


%	rdf_default_file(+Resource, -File, -NS)
%	rdf_default_file(+Resource, -File)
%	
%	Where to store facts about Resource? Should be extended to
%	include triples (or at least relations).  Default rules:
%	
%		File of the rdf:type declaration
%		File of any property on subject
%		File of resource as object
%		File of resource as property

rdf_default_file(Resource, File) :-
	rdf_default_file(Resource, File, _NS).

rdf_default_file(_, File, NS) :-
	rdfe_get_file_property(File, default(all)), !,
	rdf_default_ns(File, NS).
rdf_default_file(Resource, File, NS) :-
	(   rdf_has(Resource, rdf:type, Object, P),
	    Object \== '__not_filled',
	    rdf(Resource, P, Object, File:_)
	;   rdf(Resource, _, Object, File:_),
	    Object \== '__not_filled'
	;   rdf(_, _, Resource, File:_)
	;   rdf(_, Resource, _, File:_)
	),
	\+ rdfe_get_file_property(File, access(ro)), !,
	(   rdf_global_id(NS:_, Resource)
	->  true
	;   rdf_default_ns(File, NS)
	).
rdf_default_file(_, File, NS) :-
	rdfe_get_file_property(File, default(fallback)),
	rdf_default_ns(File, NS).

%	rdf_default_ns(+File, -NameSpace)
%	
%	Provide a default namespace  identifier  for   a  triple  to  be
%	associated with the given File.

:- dynamic
	default_ns/2.

rdf_default_ns(File, NS) :-
	default_ns(File, NS), !.
rdf_default_ns(_, t20).			% unassigned, use triple20

%	rdf_set_default_ns(+File, +Namespace)
%	
%	Set  the  default  namespace  for   this    file.   If  File  is
%	uninstantiated it will be added as a fallback clause at the end.

rdf_set_default_ns(File, NS) :-
	(   var(File)
	->  (   clause(default_ns(V, _), _, Ref),
	        var(V),
		erase(Ref),
		fail
	    ;	true
	    ),
	    assertz(default_ns(File, NS))
	;   retractall(default_ns(File, NS)),
	    asserta(default_ns(File, NS))
	).
	    

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
	(   owl_satisfies(Domain, Object)
	->  rdfe_transaction(set_object(Subject, Predicate, Object),
			     modify_property_value)
	;   throw(error(domain_error(Domain, Object), _))
	).

set_object(Subject, Predicate, Object) :-
	findall(O-Predicate, rdf_has(Subject, Predicate, O), Pairs0),
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
	(   Object == '__not_filled'
	->  File = user
	;   rdf_default_file(Subject, File)
	),
	rdfe_assert(Subject, Predicate, Object, File).


%	rdf_set_rev_object(+Subject, +Predicate, +Reverse, +New)
%	
%	Make a relation rdf(Subject, Predicate, New) and a relation
%	rdf(New, Reverse, Subject) after deleting the old relations.

rdf_set_rev_object(Subject, Predicate, Reverse, Obj) :-
	property_domain(Subject, Predicate, Domain),
	(   owl_satisfies(Domain, Obj)
	->  rdfe_transaction(set_rev_object(Subject, Predicate, Reverse, Obj),
			     modify_property_value)
	;   throw(error(domain_error(Domain, Obj), _))
	).

set_rev_object(Subject, Predicate, Reverse, Object) :-
	findall(O-Predicate, rdf_has(Subject, Predicate, O), Pairs0),
	sort(Pairs0, Pairs),
	(   Pairs = []
	->  rdf_default_file(Subject, File),
	    rdfe_assert(Subject, Predicate, Object, File),
	    rdfe_assert(Object, Reverse, Subject, File)
	;   Pairs = [Old-P|More]
	->  rdfe_update(Subject, P, Old, object(Object)),
	    ignore(rdfe_update(Old, Reverse, Subject, subject(Object))),
	    (	member(OM-PM, More),
		rdfe_retractall(Subject, PM, OM),
		rdfe_retractall(OM, Reverse, Subject),
		fail
	    ;	true
	    )
	).


%	rdf_new_property(+Subject, +Property, [Value])
%	
%	Add a dummy value for a new property on Subject that can be
%	filled by editing or drag-and-drop modification.
%
%	TBD: Check cardinality

rdf_new_property(Subject, Predicate) :-
	rdf_new_property(Subject, Predicate, _).

rdf_new_property(Subject, Predicate, Object) :-
	var(Object), !,
	(   call_rules(@display, rdf_default(Subject, Predicate, Object))
	->  true
	;   Object = '__not_filled'
	),
	rdf_new_property(Subject, Predicate, Object).
rdf_new_property(Subject, Predicate, Default) :-
	rdfe_transaction(add_object(Subject, Predicate, Default),
			 new_property).


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
	rdf_bnode(Node),
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
	forall(rdfs_subproperty_of(P, Property),
	       rdfe_retractall(Root, P, _)),
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

	
		 /*******************************
		 *	      FILES		*
		 *******************************/
	
%	rdf_merge_files(+Into, +From)
%	
%	Merge all triple that have From as their payload into Into.

rdf_merge_files(Into, From) :-
	rdfe_transaction(merge_files(Into, From),
			 merge_files(Into, From)).

merge_files(Into, From) :-
	(   rdf(S, P, O, From:Line),
	    rdfe_update(S, P, O, From:Line, source(Into)),
	    fail
	;   true
	).

		 /*******************************
		 *	      RESOURCES		*
		 *******************************/

%	rdf_change_resource(+From, +To)
%	
%	Change the resource From into To.

rdf_change_resource(To, To) :- !.
rdf_change_resource(From, To) :-
	(   (   rdf(To, _, _)
	    ;	rdf(_, To, _)
	    ;	rdf(_, _, To)
	    )
	->  send(@display, confirm, 'Target resource %s already exists.\n\
				     Do you want to merge these resources?',
		 To)
	;   true
	),
	rdfe_transaction(change_resource(From, To),
			 change_resource(From, To)).

change_resource(From, To) :-
	change_subject(From, To),
	change_predicate(From, To),
	change_object(From, To).

change_subject(From, To) :-
	S = From,
	findall(rdf(S,P,O), rdf(S, P, O), Set),
	forall(member(rdf(S,P,O), Set),
	       rdfe_update(S,P,O,subject(To))).
change_predicate(From, To) :-
	P = From,
	findall(rdf(S,P,O), rdf(S, P, O), Set),
	forall(member(rdf(S,P,O), Set),
	       rdfe_update(S,P,O,predicate(To))).
change_object(From, To) :-
	O = From,
	findall(rdf(S,P,O), rdf(S, P, O), Set),
	forall(member(rdf(S,P,O), Set),
	       rdfe_update(S,P,O,object(To))).
	    
