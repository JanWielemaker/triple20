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
:- use_module(semweb(rdf_db)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This file defines commonly used namespaces,   base ontologies. etc. Note
that the namespaces must be loaded *before*   they can be used in Prolog
source-code.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


		 /*******************************
		 *    PREDEFINED NAMESPACES	*
		 *******************************/
:- dynamic
	rdf_db:ns/2.
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
rdf_db:ns(vin,     'http://www.w3.org/2001/sw/WebOnt/guide-src/wine#').
rdf_db:ns(cyc,     'http://www.cyc.com/cyc#').
rdf_db:ns(erc,	   'http://www.swi.psy.uva.nl/mia/cml/erc#').

%	load_base_ontology(+Identifier)

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

rdf_file(rdfs,	 	 ontology('rdfs.rdfs')).
rdf_file(owl,		 ontology('owl.owl')).
rdf_file(dc,	 	 ontology('dc.rdfs')).
rdf_file(dc,	 	 ontology('eor.rdfs')).
rdf_file(vra,	 	 ontology('vra.owl')).
rdf_file(painting, 	 ontology('subject.owl')).
rdf_file(painting,	 ontology('painting.owl')).

rdf_file(aat,		 ontology('aatmeta.rdfs')).
rdf_file(aat,		 ontology('aat.rdfs')).

rdf_file(ulan,		 ontology('ulan.rdfs')).
rdf_file(ulan,		 ontology('ulan.rdf')).

rdf_file(wn,		 ontology('wordnet-20000620.rdfs')).
rdf_file(wn,		 ontology('wordnet_glossary-20010201.rdf')).
rdf_file(wn,		 ontology('wordnet_hyponyms-20010201.rdf')).
rdf_file(wn,		 ontology('wordnet_nouns-20010201.rdf')).
rdf_file(wn,		 ontology('wordnet_similar-20010201.rdf')).
rdf_file(wnrdfs,	 ontology('wnclass.rdfs')).

rdf_file(ic,		 ontology('iconclass.rdfs')).

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
requires(world,	   wnrdfs).
requires(world,	   ic).


		 /*******************************
		 *	   REQUIRED BASES	*
		 *******************************/

%	required_base_ontology(-Base)
%	
%	Deduce the required base ontologies from expressions used in the
%	document.  This is heuristic and far from complete.
%	
%	Note we first check for  the  high   level  bases  as  this will
%	automatically include the more primitive ones.

required_base_ontology(dc) :-
	(   rdf(_, _, dc:title)
	) -> true.
required_base_ontology(owl) :-
	(   rdf(_, owl:oneOf, _)
	;   rdf(_, owl:unionOf, _)
	;   rdf(_, owl:intersectionOf, _)
	;   rdf(_, owl:complementOf, _)
	;   rdf(_, _, owl:'Restriction')
	;   rdf(_, _, owl:'Class')
	;   rdf(_, _, owl:'Thing')
	) -> true.
required_base_ontology(rdfs) :-
	(   rdf(_, rdfs:subClassOf, _)
	;   rdf(_, rdf:first, _)
	;   rdf(_, rdf:rest, _)
	) -> true.


		 /*******************************
		 *		UTIL		*
		 *******************************/

expand_category(C, F) :-
	(   rdf_file(C, F)
	;   requires(C, R),
	    expand_category(R, F)
	).

