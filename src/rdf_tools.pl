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

:- module(rdf_tools,
	  [ rdf_add_missing_labels/1	% +Property
	  ]).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).
:- use_module(library('semweb/rdf_edit')).
:- use_module(rdf_util).


%	rdf_add_missing_labels(+Predicate)
%
%	Add a label using Predicate to any non-anonymous resource that has
%	no rdfs:label property.

rdf_add_missing_labels(Predicate) :-
	rdfe_transaction(add_missing_labels(Predicate),
			 add_missing_labels).

add_missing_labels(Predicate) :-
	(   rdf_subject(Subject),
	    \+ rdf_has(Subject, rdfs:label, _),
	    \+ rdf_is_bnode(Subject),
	    rdf_global_id(_:Local, Subject),
	    replace(Local,
		    [ "-" = " ",
		      "_" = " "
		    ], Label0),
	    (	rdfs_individual_of(Subject, rdfs:'Class')
	    ->	get(Label0, capitalise, Label1)
	    ;	Label1 = Label0
	    ),
	    get(Label1, strip, Label),
	    Label \== '',
	    rdf_default_file(Subject, File),
	    rdfe_assert(Subject, Predicate, literal(Label), File),
	    fail
	;   true
	).

%	replace(+In, +[From=To, ...], -Out)

replace(In, Map, Out) :-
	atom_codes(In, Codes),
	replace_codes(Map, Codes, OutCodes),
	atom_codes(Out, OutCodes).

replace_codes([], L, L).
replace_codes([In=Out|T], L0, L) :-
	replace1(L0, In, Out, L1),
	replace_codes(T, L1, L).

replace1([], _, _, []).
replace1(In, From, To, Out) :-
	append(From, After, In), !,
	replace1(After, From, To, Out0),
	append(To, Out0, Out).
replace1([H|T0], From, To, [H|T]) :-
	replace1(T0, From, To, T).
