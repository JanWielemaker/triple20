:- asserta(file_search_path(library, '.')).

:- use_module(library(rdf)).
:- use_module(rdf_db).
:- use_module(rdfs).
:- use_module(rdfs_explorer).
:- use_module(concur).

:- multifile
	rdf_db:ns/2.

rdf_db:ns(vra,  'http://www.swi.psy.uva.nl/mia/vra#').
rdf_db:ns(aat,  'http://www.swi.psy.uva.nl/mia/aat#').
rdf_db:ns(ulan, 'http://www.swi.psy.uva.nl/mia/ulan#').
rdf_db:ns(wn,   'http://www.cogsci.princeton.edu/~wn/concept#').
rdf_db:ns(wns,  'http://www.cogsci.princeton.edu/~wn/schema/').
rdf_db:ns(paint, 'http://www.swi.psy.uva.nl/mia/painting#').


rdf_file(base(rdfs),	 'rdfs.rdfs').
rdf_file(base(owl),	 'owl.owl').
rdf_file(base(dc),	 'dc.rdfs').
rdf_file(base(vra),	 'vra.owl').
rdf_file(base(painting), 'subject.owl').
rdf_file(base(painting), 'painting.owl').

rdf_file(aat,		 'aatmeta.rdfs').
rdf_file(aat,		 'aat.rdfs').

rdf_file(ulan,		 'ulan.rdfs').
rdf_file(ulan,		 'ulan.rdf').

rdf_file(wn,		 'wordnet-20000620.rdfs').
rdf_file(wn,		 'wordnet_glossary-20010201.rdf').
rdf_file(wn,		 'wordnet_hyponyms-20010201.rdf').
rdf_file(wn,		 'wordnet_nouns-20010201.rdf').
rdf_file(wn,		 'wordnet_similar-20010201.rdf').
rdf_file(wn,		 'wnclass.rdfs').

load(Category) :-				% load the whole world
	findall(rdf_load(X), rdf_file(Category, X), Goals),
	concurrent(1, Goals).

:- load(base(_)).

world :-
	load(_).

ulan :-
	load(ulan).

aat :-
	load(aat).

nav :-
	prolog_ide(open_navigator(.)).
dbg :-
	guitracer,
	debug.

%:- nav.
%:- dbg.

go :-
	new(X, rdfs_explorer),
	send(X, open).

save(X) :-
	qsave_program(X, [class(development)]).
