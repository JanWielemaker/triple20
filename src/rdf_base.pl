/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/


:- module(rdf_base,
	  [ load_base_ontology/1,	% +Id
	    current_base_ontology/1,	% -Id
	    required_base_ontology/1	% -Id
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
rdf_file(wnrdfs,	 'wnclass.rdfs').

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
requires(wnrdfs,   wn).
requires(ic,	   rdfs).

requires(world,	   owl).
requires(world,	   aat).
requires(world,	   ulan).
requires(world,	   wn).
requires(world,	   ic).


		 /*******************************
		 *	   REQUIRED BASES	*
		 *******************************/

%	required_base_ontology(-Base)
%	
%	Deduce the required base ontologies from expressions used in the
%	document.  This is heuristic.

required_base_ontology(rdfs) :-
	(   rdf(_, rdfs:subClassOf, _)
	;   rdf(_, rdf:first, _)
	;   rdf(_, rdf:rest, _)
	) -> true.
required_base_ontology(owl) :-
	(   rdf(_, owl:unionOf, _)
	;   rdf(_, owl:intersectionOf, _)
	;   rdf(_, owl:complementOf, _)
	) -> true.


		 /*******************************
		 *		UTIL		*
		 *******************************/

expand_category(C, F) :-
	(   rdf_file(C, F)
	;   requires(C, R),
	    expand_category(R, F)
	).

