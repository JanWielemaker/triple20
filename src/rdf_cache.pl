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
	    rdf_cache_clear/0,
	    rdf_cache_attach/2,		% +Cache, +Term
	    rdf_cache_detach/2,		% +Cache, -Term
	    rdf_cache_attached/2	% ?Cache, ?Term
	  ]).
:- use_module(library(debug)).
:- use_module(semweb(rdf_db)).
:- use_module(semweb(rdfs)).
:- use_module(library(broadcast)).

:- dynamic
	cache_directory/2,		% +Key, -Index
	cache_goal/3,			% +Index, +Var, -Goal
	cache_attributes/3,		% +Index, -Generation, -Size
	cache_result/2,			% +Index, -ResultSet
	cache_attached/2,		% +Index, +Satelite
	cache_empty/3,			% +Index, +Generation, Bool
	cache_statistics/3,		% +Index, -Time, -LastModified
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
	rdf_update_cache(Cache, _),
	cache_attributes(Cache, _Generation, Cardinality).

%	rdf_cache_empty(+Cache)
%	
%	Succeeds if the goal associated cache is empty. The fact is
%	cached for speedup as well as to facilitate the update thread.

rdf_cache_empty(Cache) :-
	cache_attributes(Cache, Generation, Size),
	rdf_generation(Generation), !,
	Size == 0.
rdf_cache_empty(Cache) :-
	cache_empty(Cache, Generation, Empty),
	rdf_generation(Generation), !,
	Empty == true.
rdf_cache_empty(Cache) :-
	cache_goal(Cache, _Var, Goal),
	rdf_generation(Generation), !,
	(   Goal
	->  assert(cache_empty(Cache, Generation, false)),
	    fail
	;   assert(cache_empty(Cache, Generation, true))
	).

rdf_update_empty(Cache, Modified) :-
	cache_goal(Cache, _Var, Goal),
	rdf_generation(Generation),
	(   Goal
	->  Empty = false
	;   Empty = true
	),
	(   cache_empty(Cache, _, Empty)
	->  Modified = false
	;   retract(cache_empty(Cache, _, _))
	->  assert(cache_empty(Cache, Generation, Empty)),
	    Modified = true
	;   assert(cache_empty(Cache, Generation, Empty)),
	    Modified = new
	).


%	rdf_cache_result(+Cache, ?Index, ?Result)
%	
%	Get nth result from the cache, sorted to the label-name.

rdf_cache_result(Cache, Index, Result) :-
	mutex_lock(rdf_cache),
	call_cleanup(locked_rdf_cache_result(Cache, Index, Result),
		     mutex_unlock(rdf_cache)).

locked_rdf_cache_result(Cache, Index, Result) :-
	rdf_update_cache(Cache, _Modified),
	cache_result(Cache, ResultSet),
	arg(Index, ResultSet, Result).

rdf_update_cache(Cache, false) :-
	cache_attributes(Cache, Generation, _Size),
	rdf_generation(Generation), !.
rdf_update_cache(Cache, Modified) :-
	statistics(cputime, CPU0),
	compute(Cache, Values),
	Result =.. [values|Values],

	(   cache_result(Cache, Result)
	->  RawModified = false
	;   retract(cache_result(Cache, _))
	->  assert(cache_result(Cache, Result)),
	    RawModified = true
	;   assert(cache_result(Cache, Result)),
	    RawModified = new
	),

	rdf_generation(Generation),
	functor(Result, _, Arity),
	retractall(cache_attributes(Cache, _, _)),
	assert(cache_attributes(Cache, Generation, Arity)),
	retractall(cache_empty(Cache, _, _)),

	(   Modified == false
	->  true
	;   statistics(cputime, CPU1),
	    CPU is CPU1 - CPU0,
	    get_time(Now),
	    retractall(cache_statistics(Cache, _, _)),
	    assert(cache_statistics(Cache, CPU, Now))
	),
	Modified = RawModified.

%	compute(+Cache, -ResultList)
%	
%	Computes the result-list.  By embedding Var in a term it allows
%	for various sorting and merging operations.

compute(Cache, Result) :-
	cache_goal(Cache, Var, Goal),
	var(Var), !,
	findall(Var, Goal, Result).
compute(Cache, Result) :-
	cache_goal(Cache, lsorted(Var), Goal), !,
	findall(Label-Var, (Goal, (rdfs_label(Var, Label)->true)), Values0),
	keysort(Values0, Values1),
	unique_unkey(Values1, Result).
compute(Cache, Result) :-
	cache_goal(Cache, sorted(Var), Goal), !,
	findall(Var, Goal, Values0),
	sort(Values0, Result).
compute(Cache, Result) :-
	cache_goal(Cache, Var, Goal),
	findall(Var, Goal, Result).


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
	

		 /*******************************
		 *	  ATTACH/DETACH		*
		 *******************************/

rdf_cache_attach(Cache, Satelite) :-
	asserta(cache_attached(Cache, Satelite)).

rdf_cache_detach(Cache, Satelite) :-
	retract(cache_attached(Cache, Satelite)), !.

rdf_cache_attached(Cache, Satelite) :-
	cache_attached(Cache, Satelite).


		 /*******************************
		 *	       UPDATE		*
		 *******************************/

rdf_cache_create_update_thread :-
	current_thread(rdf_cache_updater, _Status), !.
rdf_cache_create_update_thread :-
	thread_create(update_loop, _,
		      [ alias(rdf_cache_updater)
		      ]),
	listen(rdf_transaction(X),
	       thread_send_message(rdf_cache_updater,
				   rdf_transaction(X))),
	listen(rdf_undo(X),
	       thread_send_message(rdf_cache_updater,
				   rdf_transaction(X))).

update_loop :-
	repeat,
	thread_get_message(X),
	(   X = rdf_transaction(_)
	->  update_cache,
	    fail
	;   X == quit
	->  !
	).

update_cache :-
	cache_attached(Cache, _Satelite),
	(   cache_result(Cache, _)
	->  rdf_update_cache(Cache, Modified),
	    debug(rdf_cache, '~w: modified = ~w~n', [Cache, Modified])
	;   cache_empty(Cache, _, _),
	    rdf_update_empty(Cache, Modified),
	    debug(rdf_cache, '~w: empty modified = ~w~n', [Cache, Modified])
	),
	Modified == true,
	in_pce_thread(updated(Cache)),
	fail.
update_cache.

updated(Cache) :-
	(   rdf_cache_attached(Cache, Node),
	    object(Node),
	    debug(rdf_cache, '~p->update: ~w', [Node, Cache]),
	    catch(send(Node, update, Cache), E,
		  print_message(error, E)),
	    fail
	;   true
	).

:- initialization
   rdf_cache_create_update_thread.
