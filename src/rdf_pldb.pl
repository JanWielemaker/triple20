/*  $Id$

    Developed in the MIA project
    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 2000 University of Amsterdam. All rights reserved.
*/

:- module(rdf_db,
	  [ rdf/3,			% ?Subject, ?Predicate, ?Object
	    rdf/4,			% ?Subject, ?Predicate, ?Object, ?DB
	    rdf_subject/1,		% ?Subject

	    rdf_equal/2,		% ?Resource, ?Resource

	    rdf_assert/3,		% +Subject, +Predicate, +Object
	    rdf_assert/4,		% +Subject, +Predicate, +Object, +DB
	    rdf_retract/3,		% ?Subject, ?Predicate, ?Object
	    rdf_retract/4,		% ?Subject, ?Predicate, ?Object, +DB
	    rdf_retractall/3,		% ?Subject, ?Predicate, ?Object
	    rdf_retractall/4,		% ?Subject, ?Predicate, ?Object, +DB
	    rdf_update/4,		% +Subject, +Predicate, +Object, +Act

	    rdf_node/1,			% -Id

	    rdf_load/1,			% +File
	    rdf_save/1,			% +File
	    rdf_save/2,			% +File, +DB

	    rdf_source/1,		% ?File
	    rdf_make/0,			% Reload modified databases

	    rdf_source_location/2,	% +Subject, -Source
	    rdf_statistics/1,		% -Key

	    rdf_save_subject/3,		% +Stream, +Subject, +DB
	    rdf_save_header/2,		% +Out, +DB
	    rdf_save_footer/1,		% +Out

	    rdf_register_ns/2,		% +Alias, +URI
	    rdf_global_id/2,		% ?NS:Name, ?Global
	    rdf_global_term/2,		% Term, WithExpandedNS

	    rdf_match_label/3		% +How, +String, +Label
	  ]).
:- use_module(library(rdf)).


		 /*******************************
		 *	     NAMESPACES		*
		 *******************************/

:- multifile
	rdf_db:ns/2.
:- dynamic
	rdf_db:ns/2.

ns(rdf,  'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
%ns(rdfs, 'http://www.w3.org/TR/1999/PR-rdf-schema-19990303#').
ns(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').
ns(owl,  'http://www.w3.org/2002/07/owl#').
ns(xsd,  'http://www.w3.org/2000/10/XMLSchema#').
ns(dc,   'http://purl.org/dc/elements/1.1/').
ns(eor,  'http://dublincore.org/2000/03/13/eor#').

%	rdf_register_ns(+Alias, +URI)
%
%	Register a namespace.  What to do if the Alias already
%	exists?  Throw a permission error?  Use both URI's as synonyms?

rdf_register_ns(Alias, URI) :-
	ns(Alias, URI), !.
rdf_register_ns(Alias, _) :-
	ns(Alias, _),
	throw(error(permission_error(register, namespace, Alias),
		    context(_, 'Already defined'))).
rdf_register_ns(Alias, URI) :-
	assert(ns(Alias, URI)).


%	rdf_global_id(?Id, ?GlobalId)
%
%	Convert between NS:Local and global atomic identifier.
%	To be completed.

rdf_global_id(Global, Global) :-
	var(Global), !.
rdf_global_id(NS:Local, Global) :- !,
	(   ns(NS, Full)
	*-> atom_concat(Full, Local, Global)
	;   atom_concat(NS, Local, Global)
	).
rdf_global_id(Global, Global).


%	rdf_global_term(+TermIn, -GlobalTerm)
%	
%	Does rdf_global_id/2 on all terms NS:Local by recursively analysing
%	the term.

rdf_global_term(Var, Var) :-
	var(Var), !.
rdf_global_term(NS:Local, Global) :-
	rdf_global_id(NS:Local, Global).
rdf_global_term([H0|T0], [H|T]) :- !,
	rdf_global_term(H0, H),
	rdf_global_term(T0, T).
rdf_global_term(Term0, Term) :-
	compound(Term0), !,
	Term0 =.. [H|L0],
	rdf_global_term(L0, L),
	Term =.. [H|L].
rdf_global_term(Term, Term).


%	user:goal_expansion(+NSGoal, -Goal)
%	
%	This predicate allows for writing down rdf queries in a friendly
%	name-space fashion.  

:- multifile
	user:goal_expansion/2.

user:goal_expansion(rdf(Subj0, Pred0, Obj0),
		    rdf(Subj, Pred, Obj)) :-
	rdf_global_id(Subj0, Subj),
	rdf_global_id(Pred0, Pred),
	rdf_global_id(Obj0, Obj).
user:goal_expansion(rdf_assert(Subj0, Pred0, Obj0),
		    rdf_assert(Subj, Pred, Obj)) :-
	rdf_global_id(Subj0, Subj),
	rdf_global_id(Pred0, Pred),
	rdf_global_id(Obj0, Obj).
user:goal_expansion(rdf_retract(Subj0, Pred0, Obj0),
		    rdf_retract(Subj, Pred, Obj)) :-
	rdf_global_id(Subj0, Subj),
	rdf_global_id(Pred0, Pred),
	rdf_global_id(Obj0, Obj).
user:goal_expansion(rdf(Subj0, Pred0, Obj0, PayLoad),
		    rdf(Subj, Pred, Obj, PayLoad)) :-
	rdf_global_id(Subj0, Subj),
	rdf_global_id(Pred0, Pred),
	rdf_global_id(Obj0, Obj).
user:goal_expansion(rdf_assert(Subj0, Pred0, Obj0, PayLoad),
		    rdf_assert(Subj, Pred, Obj, PayLoad)) :-
	rdf_global_id(Subj0, Subj),
	rdf_global_id(Pred0, Pred),
	rdf_global_id(Obj0, Obj).
user:goal_expansion(rdf_retract(Subj0, Pred0, Obj0, PayLoad),
		    rdf_retract(Subj, Pred, Obj, PayLoad)) :-
	rdf_global_id(Subj0, Subj),
	rdf_global_id(Pred0, Pred),
	rdf_global_id(Obj0, Obj).
user:goal_expansion(rdf_equal(SubjA0, SubjB0),
		    rdf_equal(SubjA, SubjB)) :-
	rdf_global_id(SubjA0, SubjA),
	rdf_global_id(SubjB0, SubjB).
user:goal_expansion(rdf_source_location(Subj0, Source),
		    rdf_source_location(Subj, Source)) :-
	rdf_global_id(Subj0, Subj).
user:goal_expansion(rdf_subject(Subj0),
		    rdf_subject(Subj)) :-
	rdf_global_id(Subj0, Subj).


%	Special cases:
%
%	rdf_rev(-Subj, +Pred, +Obj)
%	
%	Query a compile-time known relation in backward direction. We
%	can compile this down to a straight cross-module call.

user:goal_expansion(rdf_rev(Subj0, Pred0, Obj0),
		    rdf_db:Goal) :-
	rdf_global_id(Subj0, Subj),
	rdf_global_id(Pred0, Pred),
	rdf_global_id(Obj0, Obj),
	predicate_prefix(rev, Prefix),
	atom_concat(Prefix, Pred, Name),
	Goal =.. [Name, Obj, Subj, _].
	
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Data store:

	Property(Sub, Obj, DB)
	Property$r(Obj, Sub, DB)
	Property$rl(Lit, Sub, DB)
	rdf_subject(Sub, DB)
	rdf_property(Pred, Name, ReverseName, LiteralName)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- dynamic
	rdf_subject/1,			% Subj
	rdf_property/4,			% Prop, Fwd, Reverse, RevLiteral
	rdf_source/2.			% +File, +Modified


		 /*******************************
		 *	      QUERY		*
		 *******************************/

%	rdf(?Subj, ?Pred, ?Obj)
%	rdf(?Subj, ?Pred, ?Obj, ?PayLoad)
%	
%	Query the RDF triple  database.  Note   that  the  literals  for
%	reverse  lookup  are  stored  as    lowercase  to  support  fast
%	case-insensitive  backward  search.  This   needs  some  further
%	considerations I'm afraid.
	
rdf(Subj, Pred, Obj) :-
	rdf(Subj, Pred, Obj, _).
rdf(Subj, Pred, Obj, PayLoad) :-
	rdf_property(Pred, Fwd, Rev, RevLiteral),
	(   nonvar(Subj)
	->  call(Fwd, Subj, Obj, PayLoad)
	;   nonvar(Obj)
	->  (   atom(Obj)
	    ->	call(Rev, Obj, Subj, PayLoad)
	    ;	Obj = literal(Lit)
	    ->	call(RevLiteral, Lit, Subj, PayLoad)
	    )
	;   call(Fwd, Subj, Obj, PayLoad)
	).
   

%	rdf_equal(?Resource1, ?Resource2)
%	
%	Simple equality test to exploit goal-expansion

rdf_equal(Resource, Resource).


		 /*******************************
		 *	MANIPULATING THE DB	*
		 *******************************/

%	rdf_assert(+Subject, +Pred, +Object)
%	rdf_assert(+Subject, +Pred, +Object, +Payload)
%	
%	Assert an RDF triple into the database.

rdf_assert(Subj, Pred, Obj) :-
	rdf_assert(Subj, Pred, Obj, user).
rdf_assert(Subj, Pred, Obj, PayLoad) :-
	atom(Subj), atom(Pred), rdf_object(Obj), !,
	declare_property(Pred, Fwd, Rev, RevLit),
	assert_rel(Fwd, Subj, Obj, PayLoad),
	(   atom(Obj)
	->  assert_rel(Rev, Obj, Subj, PayLoad)
	;   Obj = literal(Lit),
	    downcase_atom(Lit, Lwr),
	    assert_rel(RevLit, Lwr, Subj, PayLoad)
	),
	(   rdf_subject(Subj)
	->  true
	;   assert(rdf_subject(Subj))
	).
rdf_assert(Subj0, Pred0, Obj0, PayLoad) :-
	rdf_global_id(Subj0, Subj),
	rdf_global_id(Pred0, Pred),
	rdf_global_id(Obj0,  Obj),
	rdf_assert(Subj, Pred, Obj, PayLoad).


rdf_object(Obj) :-
	(   atom(Obj)
	->  true
	;   functor(Obj, literal, 1)
	).

%	declare_property(Pred, Fwd, Ref, RevLit)
%	
%	Declare the predicates for a new property.

declare_property(Pred, Fwd, Ref, RevLit) :-
	rdf_property(Pred, Fwd, Ref, RevLit), !.
declare_property(Pred, Fwd, Rev, RevLit) :-
	make_predicate(fwd, 3, Pred, Fwd),
	make_predicate(rev, 3, Pred, Rev),
	make_predicate(rlt, 3, Pred, RevLit),
	assert(rdf_property(Pred, Fwd, Rev, RevLit)).

predicate_prefix(fwd, '$F$').
predicate_prefix(rev, '$R$').
predicate_prefix(rlt, '$L$').

make_predicate(Table, Arity, Pred, Name) :-
	predicate_prefix(Table, Prefix),
	atom_concat(Prefix, Pred, Name),
	dynamic(Name/Arity).

assert_rel(Name, A1, A2, PayLoad) :-
	Term =.. [Name, A1, A2, PayLoad],
	assert(Term).

retract_rel(Name, A1, A2, PayLoad) :-
	Term =.. [Name, A1, A2, PayLoad],
	retract(Term).

%	rdf_retract(?Subj, ?Pred, ?Obj)
%	rdf_retract(?Subj, ?Pred, ?Obj, ?PayLoad)
%	
%	Retract one matching triple from the RDF database. This
%	predicate is non-deterministic.

rdf_retract(Subj, Pred, Obj) :-
	rdf_retract(Subj, Pred, Obj, _).
rdf_retract(Subj, Pred, Obj, PayLoad) :-
	rdf_property(Pred, Fwd, Rev, RevLit),
	retract_rel(Fwd, Subj, Obj, PayLoad),
	(   (   atom(Obj)
	    ->  retract_rel(Rev, Obj, Subj, PayLoad)
	    ;   Obj = literal(Lit)
	    ->  downcase_atom(Lit, LwrLit),
	        retract_rel(RevLit, LwrLit, Subj, PayLoad)
	    ),
	    (   \+ subject(Subj)
	    ->  retract(rdf_subject(Subj))
	    ;   true
	    )
	->  true			% avoid retract choicepoints
	).


subject(Subj) :-
	rdf_property(_Pred, Fwd, _Rev, _RevLit),
	call(Fwd, Subj, _Obj, _PayLoad).



%	rdf_retractall(?Subj, ?Pred, ?Obj)
%	rdf_retractall(?Subj, ?Pred, ?Obj, ?PayLoad)
%	
%	Retract all matching facts.

rdf_retractall(Subj, Pred, Obj) :-
	rdf_retractall(Subj, Pred, Obj, _).
rdf_retractall(Subj, Pred, Obj, PayLoad) :-
	(   rdf_retract(Subj, Pred, Obj, PayLoad),
	    fail
	;   true
	).


%	rdf_update(+Subject, +Predicate, +Object, +Action)
%	
%	Replace one of the fields of a triple.  Action is one of
%	
%		subject(+Subject)
%		predicate(+Predicate)
%		object(+Object)

rdf_update(Subject, Predicate, Object, Action) :-
	action(Action), !,
	rdf_retract(Subject, Predicate, Object, PayLoad),
	(   Action = object(New)
	->  rdf_assert(Subject, Predicate, New, PayLoad)
	;   Action = predicate(New)
	->  rdf_assert(Subject, New, Object, PayLoad)
	;   Action = subject(New)
	->  rdf_assert(New, Predicate, Object, PayLoad)
	).
rdf_update(_, _, _, Action) :-
	throw(error(domain_error(rdf_action, Action), _)).

action(object(_)).
action(subject(_)).
action(predicate(_)).


		 /*******************************
		 *	ANONYMOUS SUBJECTS	*
		 *******************************/

%	rdf_node(-Id)
%
%	Generate a unique identifier for a subject.

rdf_node(Value) :-
	repeat,
	gensym('_:', Value),
	\+ rdf_subject(Value),
	\+ rdf(_, _, Value),
	\+ rdf(_, Value, _).


		 /*******************************
		 *	      SOURCE		*
		 *******************************/

%	rdf_source_location(+Subject, -File:Line)
%	
%	Return the source-locations for triples for this subject.

rdf_source_location(Subject, Source) :-
	findall(Source, rdf(Subject, _, _, Source), Sources),
	sort(Sources, Unique),
	member(Source, Unique).


		 /*******************************
		 *	     STATISTICS		*
		 *******************************/

%	rdf_statistics(+Key(-Value))
%	
%	Obtain some statistics

rdf_statistics(sources(Count)) :-
	predicate_property(rdf_source(_,_), number_of_clauses(Count)).
rdf_statistics(subjects(Count)) :-
	predicate_property(rdf_subject(_,_), number_of_clauses(Count)).
rdf_statistics(properties(Count)) :-
	predicate_property(rdf_property(_,_,_,_), number_of_clauses(Count)).
rdf_statistics(triples(Count)) :-
	flag(rdf_triples, Old, 0),
	(   triples_on_relation(_, N),
	    flag(rdf_triples, C, C+N),
	    fail
	;   flag(rdf_triples, Total, Old)
	),
	Count = Total.
rdf_statistics(triples_by_property(Rel0, Count)) :-
	rdf_global_id(Rel0, Rel),
	triples_on_relation(Rel, Count).
rdf_statistics(triples_by_file(File, Count)) :-
	rdf_source(File),
	count_solutions(rdf(_,_,_,File:_), Count).

triples_on_relation(Rel, Count) :-
	rdf_property(Rel, Fwd, _, _),
	functor(Term, Fwd, 3),
	predicate_property(Term, number_of_clauses(Count)).

%	count_solutions(+Goal, -Count)
%	
%	Count the number of times Goal succeeds.

count_solutions(Goal, Count) :-
	flag(count_solutions, Old, 0),
	(   catch(Goal, E, (flag(count_solutions, _, Old),
			    throw(E))),
	    flag(count_solutions, C, C+1),
	    fail
	;   flag(count_solutions, C, Old)
	),
	Count = C.


		 /*******************************
		 *	    LOADING RDF		*
		 *******************************/

%	rdf_load(+File)
%
%	Load RDF file, associating each fact with File:Line

rdf_load(Spec) :-
	statistics(cputime, CpuOld),
	rdf_statistics(triples(N0)),
	(   Spec = '$stream'(_)
	->  process_rdf(Spec, [], assert_triples),
	    Load = parsed(ParseTime),
	    Action = load
	;   absolute_file_name(Spec,
			       [ access(read),
				 extensions([rdf,rdfs,owl,''])
			       ], File),
	    time_file(File, Modified),
	    (	rdf_source(File, WhenLoaded)
	    ->	(   Modified > WhenLoaded
		->  rdf_retractall(_,_,_,File:_),
		    Action = reload
		;   Action = none
		)
	    ;	Action = load
	    ),
	    (	Action \== none
	    ->	retractall(rdf_source(File, _)),
		assert(rdf_source(File, Modified)),
		(   cache_file(File, Cache)
		->  (   time_file(Cache, CacheTime),
		        time_file(File, FileTime),
			CacheTime >= FileTime,
			catch(open(Cache, read, CacheStream), _, fail)
		    ->  load_triples_from_stream(CacheStream),
			close(CacheStream),
			Load = cache(ParseTime)
		    ;   catch(open(Cache, write, CacheStream), _, fail)
		    ->  process_rdf(File, [], cache_triples(CacheStream)),
			close(CacheStream),
			Load = parsed(ParseTime)
		    ;   process_rdf(File, [], assert_triples),
			Load = parsed(ParseTime)
		    )
		;   process_rdf(File, [], assert_triples),
		    Load = parsed(ParseTime)
		)
	    ;	true
	    )
	),
	(   Action \== none
	->  rdf_statistics(triples(N1)),
	    statistics(cputime, CpuLoaded),
	    ParseTime is CpuLoaded - CpuOld,
	    N is N1 - N0,
	    print_message(informational,
			  rdf(loaded(Spec, N, Load)))
	;   true
	).


%	rdf_source(?Source)
%	
%	Query the loaded sources

rdf_source(File) :-
	rdf_source(File, _).

%	rdf_make
%	
%	Reload all loaded files that have been modified since the last
%	time they were loaded.

rdf_make :-
	forall(rdf_source(File, _Time),
	       rdf_load(File)).


%	cache_file(+Base, -CacheFile)
%	
%	Deduce the name of the file used to cache the triples.

cache_file(Base, Cache) :-
	file_directory_name(Base, BaseDir),
	file_base_name(Base, File),
	atom_concat(BaseDir, '/.cache', CacheDir),
	exists_directory(CacheDir),
	concat_atom([CacheDir, /, File, '.pl'], Cache).


%	cache_triples(+Stream, +Triples, PayLoad)

cache_triples(Stream, Triples, PayLoad) :-
	cache_triples2(Triples, Stream, PayLoad).

cache_triples2([], _, _) :- !.
cache_triples2([rdf(S,P,O)|T], Stream, PayLoad) :- !,
	rdf_global_id(S, Subject),
	rdf_global_id(P, Predicate),
	rdf_global_id(O, Object),
	rdf_assert(Subject, Predicate, Object, PayLoad),
	format(Stream, '~q.~n', [rdf(Subject, Predicate, Object, PayLoad)]),
	cache_triples2(T, Stream, PayLoad).
cache_triples2([H|_], _, _) :-
	throw(error(type_error(rdf_triple, H), _)).


%	assert_triples(+Triples, +Source)
%
%	Assert a list of triples into the database. Foir security
%	reasons we check we aren't inserting anything but nice RDF
%	triples.

assert_triples([], _).
assert_triples([rdf(S,P,O)|T], DB) :- !,
	rdf_assert(S, P, O, DB),
	assert_triples(T, DB).
assert_triples([H|_], _) :-
	throw(error(type_error(rdf_triple, H), _)).

%	load_triples_from_stream(+PayLoad, +Stream)
%	
%	Read triples from a file and assert them into the database.

load_triples_from_stream(In) :-
	read(In, T0),
	load_triples_from_stream(T0, In).

load_triples_from_stream(end_of_file, _) :- !.
load_triples_from_stream(rdf(S,P,O,DB), In) :- !,
	rdf_assert(S, P, O, DB),
	read(In, T),
	load_triples_from_stream(T, In).
load_triples_from_stream(T, _) :-
	throw(error(type_error(rdf_triple, T), _)).


		 /*******************************
		 *	     SAVE RDF		*
		 *******************************/

%	rdf_save(File)
%
%	Save RDF data to file

rdf_save(File) :-
	rdf_save(File, _).

rdf_save(File, DB) :-
	open(File, write, Out),
	rdf_save_header(Out, DB),
	forall(rdf_subject(Subject, DB),
	       rdf_save_non_anon_subject(Out, Subject, DB)),
	rdf_save_footer(Out),
	close(Out).

%	rdf_save_header(+Fd, +DB)
%
%	Save XML documentheader, doctype and open the RDF environment.
%	This predicate also sets up the namespace notation.

rdf_save_header(Out, DB) :-
	format(Out, '<?xml version=\'1.0\' encoding=\'ISO-8859-1\'?>~n', []),
	format(Out, '<!DOCTYPE rdf:RDF [', []),
	used_namespaces(NSList, DB),
	(   member(Id, NSList),
	    ens(Id, NS),
	    format(Out, '~N    <!ENTITY ~w \'~w\'>', [Id, NS]),
	    fail
	;   true
	),
	format(Out, '~N]>~n~n', []),
	format(Out, '<rdf:RDF', []),
	(   member(Id, NSList),
	    format(Out, '~N    xmlns:~w="&~w;"~n', [Id, Id]),
	    fail
	;   true
	),
	format(Out, '>~n', []).

ens(e, '').
ens(Id, NS) :-
	ns(Id, NS).

%	used_namespaces(-List)
%
%	Return the list of namespaces used in an RDF database.

:- dynamic
	used_ns/1.

used_namespaces(List, DB) :-
	setof(NS, Full^ens(NS, Full), NS0),
	used_ns(NS0, List, DB).

used_ns([], [], _).
used_ns([H|T0], [H|T], DB) :-
	used_ns(H, DB), !,
	used_ns(T0, T, DB).
used_ns([_|T0], T, DB) :-
	used_ns(T0, T, DB).

used_ns(e, _) :- !.			% for now just assume it
used_ns(NS, DB) :-
	ns(NS, Full),
	rdf_db(S,P,O,DB),
	(   sub_atom(S, 0, _, _, Full)
	;   sub_atom(P, 0, _, _, Full)
	;   atom(O),
	    sub_atom(O, 0, _, _, Full)
	), !.


rdf_save_footer(Out) :-
	format(Out, '</rdf:RDF>~n', []).

rdf_save_non_anon_subject(_Out, Subject, _DB) :-
	anonymous_subject(Subject), !.
rdf_save_non_anon_subject(Out, Subject, DB) :-
	rdf_save_subject(Out, Subject, DB).


rdf_save_subject(Out, Subject, DB) :-
	rdf_save_subject(Out, Subject, rdf, 0, DB),
	format(Out, '~n', []).

rdf_save_subject(Out, Subject, DefNS, Indent, DB) :-
	setof(Pred=Object, rdf_db(Subject, Pred, Object, DB), Atts),
	rdf_save_subject(Out, Subject, DefNS, Atts, Indent, DB).

rdf_save_subject(Out, Subject, DefNS0, Atts, Indent, DB) :-
	rdf_global_id(rdf:type, RdfType),
	select(RdfType=Type, Atts, Atts1), !,
	rdf_local_id(Type, DefNS0, DefNS, TypeId),
	format(Out, '~*|<~w', [Indent, TypeId]),
	save_about(Out, Subject, Indent),
	save_attributes(Atts1, DefNS, Out, TypeId, Indent, DB).
rdf_save_subject(Out, Subject, _DefNS, Atts, Indent, DB) :-
	format(Out, '~*|<rdf:Description', [Indent]),
	save_about(Out, Subject, Indent),
	save_attributes(Atts, rdf, Out, rdf:'Description', Indent, DB).

save_about(_Out, Subject, Indent) :-
	Indent > 0,
	anonymous_subject(Subject), !.
save_about(Out, Subject, _) :-
	rdf_value(Subject, QSubject),
	format(Out, ' rdf:about="~w"', [QSubject]).

%	save_attributes(+List, +DefNS, +Stream, Element)
%
%	Save the attributes.  Short literal attributes are saved in the
%	tag.  Others as the content of the description element.  The
%	begin tag has already been filled.

save_attributes(Atts, DefNS, Out, Element, Indent, DB) :-
	split_attributes(Atts, InTag, InBody),
	SubIndent is Indent + 2,
	save_attributes2(InTag, DefNS, tag, Out, SubIndent, DB),
	(   InBody == []
	->  format(Out, '/>~n', [])
	;   format(Out, '>~n', []),
	    save_attributes2(InBody, _, body, Out, SubIndent, DB),
	    format(Out, '~N~*|</~w>~n', [Indent, Element])
	).

%	split_attributes(+Attributes, -Inline, -Body)
%
%	Split attributes for (literal) attributes to be used in the
%	begin-tag and ones that have to go into the body of the description.

split_attributes([], [], []).
split_attributes([H|TA], [H|TI], B) :-
	in_tag_attribute(H), !,
	split_attributes(TA, TI, B).
split_attributes([H|TA], I, [H|TB]) :-
	split_attributes(TA, I, TB).

in_tag_attribute(_=literal(Text)) :-
	atom_length(Text, Len),
	Len < 60.

%	save_attributes(+List, +DefNS, +TagOrBody, +Stream)
%
%	Save a list of attributes.

save_attributes2([], _, _, _, _, _).
save_attributes2([H|T], DefNS, Where, Out, Indent, DB) :-
	save_attribute(Where, H, DefNS, Out, Indent, DB),
	save_attributes2(T, DefNS, Where, Out, Indent, DB).

save_attribute(tag, Name=literal(Value), DefNS, Out, Indent, _DB) :-
	rdf_id(Name, DefNS, NameText),
	xml_quote_attribute(Value, QVal),
	format(Out, '~N~*|~w="~w"', [Indent, NameText, QVal]).
save_attribute(body, Name=literal(Value), DefNS, Out, Indent, _DB) :- !,
	rdf_local_id(Name, DefNS, NameText),
	xml_quote_cdata(Value, QVal),
	format(Out, '~N~*|<~w>~w</~w>', [Indent, NameText, QVal, NameText]).
save_attribute(body, Name=Value, DefNS0, Out, Indent, DB) :-
	anonymous_subject(Value), !,
	rdf_local_id(Name, DefNS0, DefNS, NameText),
	format(Out, '~N~*|<~w>~n', [Indent, NameText]),
	SubIndent is Indent + 2,
	rdf_save_subject(Out, Value, DefNS, SubIndent, DB),
	format(Out, '~N~*|</~w>~n', [Indent, NameText]).
save_attribute(body, Name=Value, DefNS, Out, Indent, _DB) :-
	rdf_value(Value, QVal),
	rdf_local_id(Name, DefNS, NameText),
	format(Out, '~N~*|<~w rdf:resource="~w"/>', [Indent, NameText, QVal]).

anonymous_subject(S) :-
	sub_atom(S, 0, _, _, 'Description__').


rdf_id(Id, NS, NS:Local) :-
	ns(NS, Full),
	Full \== '',
	atom_concat(Full, Local, Id), !.
rdf_id(Id, e, e:Id).


rdf_local_id(Id, NS, Local) :-
	ns(NS, Full),
	atom_concat(Full, Local, Id), !.
rdf_local_id(Id, _, Text) :-
	rdf_id(Id, _, Text).

rdf_local_id(Id, NS, NS, Local) :-
	ns(NS, Full),
	atom_concat(Full, Local, Id), !.
rdf_local_id(Id, _, NS, Text) :-
	rdf_id(Id, NS, Text).

rdf_value(V, Text) :-
	ns(NS, Full),
	atom_concat(Full, Local, V), !,
	concat_atom(['&', NS, (';'), Local], Text).
rdf_value(V, V).


%	rdf_match_label(+Method, +String, +Label)
%	
%	Match a literal with a string.  Matching is case-insensitive

rdf_match_label(exact, Text, Text) :- !.
rdf_match_label(Method, Sub, Text) :-
	'$apropos_match'(Sub, Text),
	(   Method == substring
	->  true
	;   Method == prefix
	->  downcase_atom(Text, Lwr),
	    sub_atom(Lwr, 0, _, _, Sub)
	;   Method == word
	->  word_in_string(Sub, Text)
	).

%	word_in_string(+Word, +String)
%	
%	Succeeds if Word appears in String surrounded by non-alnum
%	characters or the end of String.

word_in_string(Word, String) :-
	sub_atom(String, Pre, _, Post, Word),
	(   Pre == 0
	->  true
	;   Pre1 is Pre - 1,
	    sub_atom(String, Pre1, 1, _, Before),
	    \+ char_type(Before, alnum)
	),
	(   Post == 0
	->  true
	;   Post1 is Post - 1,
	    sub_atom(String, _, 1, Post1, After),
	    \+ char_type(After, alnum)
	), !.


		 /*******************************
		 *	       MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(rdf(loaded(Spec, Triples, parsed(ParseTime)))) -->
	{   atom(Spec)
	->  file_base_name(Spec, Base)
	;   Base = Spec
	},
	[ 'Parsed "~w" in ~2f sec; added ~D triples'-
	  [Base, ParseTime, Triples]
	].
prolog:message(rdf(loaded(Spec, Triples, cache(ParseTime)))) -->
	{   atom(Spec)
	->  file_base_name(Spec, Base)
	;   Base = Spec
	},
	[ 'Loaded "~w" in ~2f sec; added ~D triples'-
	  [Base, ParseTime, Triples]
	].

