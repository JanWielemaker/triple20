wn_concept1(X) :-
	wn_concept1(X, 'http://www.cogsci.princeton.edu/~wn/concept#100001740').

wn_concept1(X, X).
wn_concept1(C, S) :-
	rdf_has(Y, rdfs:subClassOf, S),
	wn_concept1(C, Y).
	

wn_concept2(X) :-
	wn_concept2(X, 'http://www.cogsci.princeton.edu/~wn/concept#100001740').

wn_concept2(X, X).
wn_concept2(C, S) :-
	rdf_has(Y, wns:hyponymOf, S),
	wn_concept2(C, Y).
	
get_type(N) :-
	nouns(Nouns),
	functor(Nouns, _, Arity),
	time((   between(1, N, _),
		 I is random(Arity)+1,
		 arg(I, Nouns, Noun),
%		 rdf(Noun, rdf:type, _),
		 fail
	     ;	 true
	     )).

nouns(Nouns) :-
	findall(X, rdf(X, rdf:type, wns:'Noun'), Xs),
	Nouns =.. [nouns|Xs].
