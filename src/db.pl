:- module(rdf_db,
	  [ rdf_assert/3,		% +Subject, +Predicate, +Object
	    rdf_assert/4,		% +Subject, +Predicate, +Object, +Source
	    rdf/3,			% ?Subject, ?Predicate, ?Object
	    rdf/4,			% ?Subject, ?Predicate, ?Object, ?Source
	    rdf_retractall/3,		% ?Subject, ?Predicate, ?Object
	    rdf_retractall/4,		% ?Subject, ?Predicate, ?Object, ?Source
	    rdf_statistics/1,		% ?Key
	    assert/0,
	    as/0,
	    test/0
	  ]).

:- initialization
   load_foreign_library(rdf_db).

assert :-
	rdf_assert(x, y, z),
	rdf_assert(x, y, literal(z)).

as :-
	rdf_assert(a,b,c),
	rdf_assert(a,b,d),
	rdf_assert(a,b,e, file:1).

test :-
	t(rdf(x,y,z), true),
	t(Z, rdf(x,y,Z), [z, literal(z)]).

t(Goal, true) :-
	findall(Goal, Goal, Solutions),
	Solutions = [Solution],
	Goal =@= Solution.

t(Var, Goal, OK) :-
	findall(Var, Goal, Solutions),
	OK =@= Solutions.
