#!/staff/jan/bin/pl -G32m -T32m -L16m -s

:- dynamic
	rdf_db:ns/2.
:- multifile
	rdf_db:ns/2.

rdf_db:ns(cyc,	   'http://www.cyc.com/cyc#').

:- [load].

link_root :-
	rdfe_transaction((rdfs_individual_of(X, rdfs:'Class'),
			  \+ rdf_has(X, rdfs:subClassOf, _),
			  rdfe_assert(X, rdfs:subClassOf, rdfs:'Resource'),
			  fail;
			 true)).

cyc :-
	go(['cyc.rdfj', 'cyc.rdfs']).
%	link_root.

:- cyc.

