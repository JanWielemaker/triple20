/*  File:    rdf_cache.pl
    Author:  Jan Wielemaker
    Created: Jul 18 2003
    Purpose: Caching results
*/

:- module(rdf_cache,
	  [ rdf_cache/3,		% +Var, :Goal, -Cache
	    rdf_cache_cardinality/2,	% +Cache, -Cardinality
	    rdf_cache_result/3,		% +Cache, ?Index, -Value
	    rdf_cache_empty/1,		% +Cache
	    rdf_cache_clear/1,		% +Cache
	    rdf_cache_clear/0
	  ]).
:- use_module(semweb(rdf_db)).
:- use_module(semweb(rdfs)).

:- dynamic
	cache_directory/2,		% +Key, -Index
	cache_goal/3,			% +Index, +Var, -Goal
	cache_attributes/3,		% +Index, -Generation, -Size
	cache_result/2,			% +Index, -ResultSet
	next_cache/1.			% +Index

:- meta_predicate
	rdf_cache(+, :, -).

user:goal_expansion(rdf_cache(Var, Goal, Index),
		    rdf_cache(Var, Goal2, Index)) :-
	expand_goal(Goal, Goal2).

%	rdf_cache(+Var, :Goal, -Cache)
%	
%	Find or allocate a cache for Var in Goal.

rdf_cache(Var, GoalSpec, Index) :-
	strip_module(GoalSpec, Module, Goal),
	with_mutex(rdf_cache, 
		   rdf_cache:rdf_cache2(Var, Module:Goal, Index)).

rdf_cache2(Var, Goal, Index) :-
	copy_term(Var=Goal, Key),
	numbervars(Key, 0, _),
	(   cache_directory(Key, I)
	->  true
	;   (   retract(next_cache(I))
	    ->  I2 is I + 1,
		assert(next_cache(I2))
	    ;   I = 1,
		assert(next_cache(2))
	    ),
	    assert(cache_directory(Key, I)),
	    assert(cache_goal(I, Var, Goal))
	),
	Index = I.
	    
	    
%	rdf_cache_cardinality(+Cache, -Cardinality)
%	
%	Get the size of the result-set.

rdf_cache_cardinality(Cache, Cardinality) :-
	rdf_update_cache(Cache),
	cache_attributes(Cache, _Generation, Cardinality).

%	rdf_cache_empty(+Cache)
%	
%	Succeeds if the goal associated cache is empty

rdf_cache_empty(Cache) :-
	cache_attributes(Cache, Generation, Size),
	rdf_generation(Generation), !,
	Size == 0.
rdf_cache_empty(Cache) :-
	cache_goal(Cache, _Var, Goal),
	\+ Goal.

%	rdf_cache_result(+Cache, ?Index, ?Result)
%	
%	Get nth result from the cache, sorted to the label-name.

rdf_cache_result(Cache, Index, Result) :-
	rdf_update_cache(Cache),
	cache_result(Cache, ResultSet),
	arg(Index, ResultSet, Result).


rdf_update_cache(Cache) :-
	cache_attributes(Cache, Generation, _Size),
	rdf_generation(Generation), !.
rdf_update_cache(Cache) :-
	retractall(cache_attributes(Cache, _, _)),
	retractall(cache_result(Cache, _)),
	cache_goal(Cache, Var, Goal),
	findall(Label-Var, (Goal, (rdfs_label(Var, Label)->true)), Values0),
	keysort(Values0, Values1),
	unique_unkey(Values1, Values),
	Result =.. [values|Values],
	assert(cache_result(Cache, Result)),
	rdf_generation(Generation),
	functor(Result, _, Arity),
	assert(cache_attributes(Cache, Generation, Arity)).

unique_unkey([], []).
unique_unkey([H0|T0], [H|T]) :-
	remove_dups(H0, T0, T1),
	H0 = _Key-H,
	unique_unkey(T1, T).

remove_dups(H, [H|T0], T) :- !,
	remove_dups(H, T0, T).
remove_dups(_, L, L).


%	rdf_cache_clear(+Cache)
%
%	Empty the cache with given id.

rdf_cache_clear(Cache) :-
	retractall(cache_attributes(Cache, _, _)),
	retractall(cache_result(Cache, _)).

rdf_cache_clear :-
	(   retract(cache_attributes(Cache, _Generation, _Size)),
	    retractall(cache_result(Cache, _)),
	    fail
	;   true
	).
	
