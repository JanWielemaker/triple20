/*  File:    rdf_base.pl
    Author:  Jan Wielemaker
    Created: Sep 22 2003
    Purpose: Information about our base-set of ontologies
*/

:- module(rdf_base,
	  [ load_base_ontology/1,	% +Id
	    current_base_ontology/1	% -Id
	  ]).
:- use_module(concur).

%	load_base_ontoloty(+Identifier)

load_base_ontology(Category) :-
	load_base_ontology(1, Category).

load_base_ontology(C, Category) :-
	findall(rdfe_load(X), expand_category(Category, X), Goals0),
	list_to_set(Goals0, Goals),
	concurrent(C, Goals).

%	current_base_ontology(-Identifier)
%	
%	Enemate defined base-ontologies

current_base_ontology(Id) :-
	findall(X, (rdf_file(X, _);requires(X, _)), Xs),
	sort(Xs, List),
	member(Id, List).

%	rdf_file(+Identifier, -File)
%	
%	Register the file that belong to a base ontology

rdf_file(rdfs,	 	 'rdfs.rdfs').
rdf_file(owl,		 'owl.owl').
rdf_file(dc,	 	 'dc.rdfs').
rdf_file(vra,	 	 'vra.owl').
rdf_file(painting, 	 'subject.owl').
rdf_file(painting,	 'painting.owl').

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

%	requires(+Id1, -Id2)
%	
%	Base Id1 requires base Id2.

requires(owl,	   rdfs).
requires(dc,	   rdfs).
requires(vra,	   owl).
requires(painting, owl).
requires(aat,	   rdfs).
requires(ulan,	   rdfs).
requires(wn,	   rdfs).
requires(ic,	   rdfs).

requires(world,	   owl).
requires(world,	   aat).
requires(world,	   ulan).
requires(world,	   wn).
requires(world,	   ic).


		 /*******************************
		 *		UTIL		*
		 *******************************/

expand_category(C, F) :-
	(   rdf_file(C, F)
	;   requires(C, R),
	    expand_category(R, F)
	).

