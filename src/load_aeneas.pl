
:- load_files([load]).

:- ['owl_inference.pl'].

:- set_feature(history,200).
:- rdf_load('wordnet-20000620.rdfs').
:- rdf_load('wnclass.rdfs').

%:-rdf_load('aeneas.rdf').
:- rdf_load('aeneas.rdfs').

user:portray(URL) :-
	atom(URL),
	rdf_has(URL, rdfs:label, literal(Label), _),
	format('~q (~w)', [URL, Label]).
	

rdf_db:ns(ghs, 'http://www.swi.psy.uva.nl/mia/ghs#').

:- load(wn).

test(P) :-
	rdf_has(ghs:'Aeneas', ghs:has_parent, P).

t2(S) :-
	rdf_reachable(S,
		      rdfs:subClassOf,
		      'http://www.cogsci.princeton.edu/~wn/concept#106861622').
owl_test(P) :-
	owl_property(ghs:'Aeneas', ghs:has_parent, P).
