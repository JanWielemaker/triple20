#!/staff/jan/bin/pl -G32m -T32m -L16m -s

:- dynamic
	rdf_db:ns/2.
:- multifile
	rdf_db:ns/2.

rdf_db:ns(cyc,	   'http://www.cyc.com/cyc#').

:- [load].

cyc :-
	go(['--reset', 'cyc.rdfj', 'cyc.rdfs']).

:- cyc.

