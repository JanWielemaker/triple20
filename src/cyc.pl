#!/staff/jan/bin/pl -G32m -T32m -L16m -s

:- dynamic
	rdf_db:ns/2.
:- multifile
	rdf_db:ns/2.

rdf_db:ns(cyc,	   'http://www.cyc.com/cyc#').

:- [load].

cyc :-
	current_prolog_flag(argv, Argv),
	append(_, [--|Args], Argv),
	append(['cyc.rdfj', 'cyc.rdfs'], Args, AllArgs),
	go(AllArgs).

:- cyc.

