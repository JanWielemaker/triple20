/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2005, University of Amsterdam

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

:- module(rdf_minimise,
	  [ rdf_minimise_inverse/0
	  ]).
:- use_module(library(pce)).
:- use_module(library(lists)).
:- use_module(rdf_rules).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).
:- use_module(library('semweb/rdf_edit')).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The purpose of this module is to   remove  redundant triples and convert
the database in a more canonical form.  The operations provided are:

	* rdf_minimise_inverse
	Delete redundant triples can can be inferred through 

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%	rdf_minimise_inverse/0
%	
%	Remove redundant reverse triples.  It performs the following steps:
%	
%		- Determine the inverse property pairs
%		- Determine the preferred property.  This is defined
%		  to be the property that has the lowest branching factor,
%		  so properties generally go from subjects to their collection.

rdf_minimise_inverse :-
	findall(P-I, rdf_has(P, owl:inverseOf, I), P0),
	unique_ip(P0, P1),
	ip_with_triples(P1, P2),
	maplist(ip_preferred, P2, P3),
	send(rdf_inverse_dialog(P3), open_centered).

%	unique_ip(+IP-Pairs, -Unique)
%	
%	Remove all duplicate specifications, so we end up with a list of
%	unique inverse property pairs.

unique_ip([], []).
unique_ip([P-I|Set], Unique) :-
	(   (   memberchk(I-P, Set)
	    ;   memberchk(P-I, Set)
	    )
	->  unique_ip(Set, Unique)
	;   Unique = [P-I|Rest],
	    unique_ip(Set, Rest)
	).
	

%	ip_with_triples(+PairsIn, -WithTriples)
%	
%	Determine the relations that actually have triples on either
%	relation.  If not, there is nothing to clean.

ip_with_triples([], []).
ip_with_triples([H|T0], [H|T]) :-
	H = P-I,
	(   rdf_predicate_property(I, triples(N))
	;   rdf_predicate_property(P, triples(N))
	),
	N > 0, !,
	ip_with_triples(T0, T).
ip_with_triples([_|T0], T) :-
	ip_with_triples(T0, T).


%	ip_preferred(+Pair, -PreferredPair)
%	
%	PreferredPair is P-I, where P is the relation we want to store
%	and I is the relation we want to infer.

ip_preferred(I-P, P-I) :-
	rdf_predicate_property(I, triples(IN)),
	rdf_predicate_property(P, triples(PN)),
	(   IN > PN, IN > 10
	->  rdf_predicate_property(I, rdf_subject_branch_factor(IB)),
	    rdf_predicate_property(I, rdf_object_branch_factor(OB)),
	    IB < OB
	;   PN > IN, PN > 10
	->  rdf_predicate_property(P, rdf_subject_branch_factor(IB)),
	    rdf_predicate_property(P, rdf_object_branch_factor(OB)),
	    OB < IB
	), !.
ip_preferred(P-I, I-P).


minimise_inverse([]).
minimise_inverse([P-I|T]) :-
	findall(t(S,O,DB), (rdf(S,I,O,DB),atom(O)), Pairs),
	(   member(t(S,O,DB), Pairs),
	    rdfe_retractall(S,I,O,DB),
	    (	rdf(O,P,I)
	    ->  true
	    ;   rdfe_assert(O,P,I,DB)
	    ),
	    fail
	;   true
	).


		 /*******************************
		 *	      GUI		*
		 *******************************/

:- pce_begin_class(rdf_inverse_dialog, rdf_dialog,
		   "Query inverse relations").

initialise(D, Pairs:prolog) :->
	send_super(D, initialise, @default, 'Canonise inverse relations'),
	send(D, append,
	     label(comment,
		   'After clicking clean, the database is cleaned such\n\
		    that only triples of the left property appear')),
	send(D, append, graphical(width:=10, height:=10)),
	forall(member(P-I, Pairs),
	       send(D, append, rdf_inverse_pair(P, I))),
	send(D, append, graphical(width:=10, height:=10)),
	send(D, standard_buttons, clean),
	get(D, member, clean, B),
	send(B, active, @on).

selection(D, Pairs:prolog) :<-
	get(D?graphicals, find_all,
	    message(@arg1, instance_of, rdf_inverse_pair),
	    Grs),
	chain_list(Grs, List),
	selections(List, Pairs).

selections([], []).
selections([H0|T0], [H|T]) :-
	get(H0, selection, H),
	selections(T0, T).

clean(D) :->
	get(D, selection, Pairs),
	rdfe_transaction(minimise_inverse(Pairs), minimise_inverse).

:- pce_end_class(rdf_inverse_dialog).

:- pce_begin_class(rdf_inverse_pair, device,
		   "Allow changing a relation").

initialise(D, P:name, I:name) :->
	send_super(D, initialise),
	send(D, format, format(horizontal, 3, @on)),
	call_rules(D, label(I, IL)),
	call_rules(D, label(P, PL)),
	send(D, display, PL),
	send(D, display, new(Swap, button(swap, message(D, swap)))),
	send(Swap, label, '<=>'),
	send(D, display, IL).

reference(D, Ref:point) :<-
	get(D, member, swap, B),
	get(B, center, Ref).

selection(D, Pair:prolog) :<-
	"Get selection as a pair"::
	get_chain(D, graphicals, [PL, _, IL]),
	get(IL, resource, I),
	get(PL, resource, P),
	Pair = P-I.

swap(D) :->
	"Swap the two properties"::
	get_chain(D, graphicals, [PL, _, IL]),
	send(PL, expose),
	send(IL, hide).

:- pce_end_class(rdf_inverse_pair).
