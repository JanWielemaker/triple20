:- asserta(file_search_path(library, '.')).

:- load_files([ rdf_tree,
		rdf_db,
		concur
	      ],
	      [ silent(true)
	      ]).

:- pce_image_directory(icons).

:- multifile
	rdf_db:ns/2.

rdf_db:ns(vra,	   'http://www.swi.psy.uva.nl/mia/vra#').
rdf_db:ns(aat,	   'http://www.swi.psy.uva.nl/mia/aat#').
rdf_db:ns(ulan,	   'http://www.swi.psy.uva.nl/mia/ulan#').
rdf_db:ns(wn,	   'http://www.cogsci.princeton.edu/~wn/concept#').
rdf_db:ns(wns,	   'http://www.cogsci.princeton.edu/~wn/schema/').
rdf_db:ns(paint,   'http://www.swi.psy.uva.nl/mia/painting#').
rdf_db:ns(subject, 'http://www.swi.psy.uva.nl/mia/subject#').
rdf_db:ns(ic,	   'http://www.swi.psy.uva.nl/mia/iconclass#').
rdf_db:ns(ghs,	   'http://www.swi.psy.uva.nl/mia/ghs#').

rdf_file(base(rdfs),	 'rdfs.rdfs').
rdf_file(base(owl),	 'owl.owl').
rdf_file(base(dc),	 'dc.rdfs').
rdf_file(base(vra),	 'vra.owl').
rdf_file(base(painting), 'subject.owl').
rdf_file(base(painting), 'painting.owl').
rdf_file(base(tool),	 'tool.owl').
rdf_file(base(tool),	 'tool.rdf').

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

rdf_file(ic,		 'iconclass.rdfs').

load(Category) :-
	load(1, Category).
load(C, Category) :-				% load the whole world
	findall(rdf_load(X), rdf_file(Category, X), Goals),
	concurrent(C, Goals).

% :- rdf_debug(1).		% Print messages
:- load(base(rdfs)).
:- load(base(owl)).
:- load(base(tool)).

world :-
	load(_).

ulan :-
	load(ulan).

aat :-
	load(aat).

dbg :-
%	debug(render),
	guitracer.

%:- nav.
%:- dbg.

go :-
	new(P, picture),
	send(P, display, new(T, rdf_tree)),
	send(T, expand_root),
	send(P, open).
