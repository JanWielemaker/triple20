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

:- module(particle,
	  [ begin_particle/2,		% +Name, +Supers
	    begin_particle/3,		% +Name, +Supers, +File:Line
	    end_particle/0,
	    current_particle/1,		% ?Particle
	    (::)/2			% +Particle, +Goal
	  ]).
:- use_module(library(lists)).

:- op(600, xfy, user:(::)).
:- op(600, fy,  user:(::)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Extremely lightweight package to do use modules more like classes. It
allows of defining multiple modules in a single file between:

	:- begin_particle(Name, Super).

	:- end_particle.

Predicates are inherited from the Super  particle. The following special
calling primitives are provided:

	# Particle::Goal
	Call Goal in Particle, setting the notion of the `self' particle
	to Particle.

	# ::Goal
	When called inside a particle, call Goal in the `self' particle.

	# super::Goal
	When called inside a particle, call Goal in the Super particle.

	# Particle:Goal
	Call Goal in Particle, not changing the notion of `self'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- dynamic
	loading_particle/2,
	current_particle/2.		% defined particles.


		 /*******************************
		 *	    DECLARATION		*
		 *******************************/

begin_particle(Name, _) :-
	current_module(Name),
	\+ current_particle(Name, _),
	throw(error(permission_error(create_particle, Name),
		    'Existing module')).
begin_particle(Name, Super0) :-
	source_location(File, Line),
	begin_particle(Name, Super0, File:Line).
begin_particle(Name, Super0, File:Line) :-
	canonical_super(Super0, Supers),
	(   current_particle(Name, Supers)
	->  true
	;   retractall(current_particle(Name, _)),
	    assert(current_particle(Name, Supers)),
	    set_import_modules(Name, Supers)
	),
	'$set_source_module'(Old, Name),
	'$declare_module'(Name, File, Line),
	asserta(loading_particle(Name, Old)).

end_particle :-
	retract(loading_particle(_, Old)), !,
	'$set_source_module'(_, Old).

canonical_super(X, X) :-
	is_list(X), !.
canonical_super(X, [X]).

set_import_modules(Module, Imports) :-
	findall(I, import_module(Module, I), IL),
	forall(member(I, IL), delete_import_module(Module, I)),
	forall(member(I, Imports), add_import_module(Module, I, end)),
	'$set_source_module'(Context, Context),
	add_import_module(Module, Context, end).


		 /*******************************
		 *	     SUPER/SELF		*
		 *******************************/

:- multifile
	user:term_expansion/2,
	user:goal_expansion/2.

user:term_expansion((:- end_particle),
		    (:- particle:end_particle)).

user:goal_expansion(super::G, Expanded) :-
	(   loading_particle(L, _)
	->  current_particle(L, Supers),
	    (   Supers = [S]
	    ->	(   current_particle(S)
		->  true
		;   throw(error(existence_error(particle, S), super))
		),
		(   current_predicate(_, S:G)
		->  Expanded = S:G
		;   Expanded = call_outer(G)
		)
	    ;	Supers == []
	    ->	Expanded = call_outer(G)
	    ;	throw(error(ambiguous(super, Supers), _))
	    )
	).
user:goal_expansion(outer::G, call_outer(G)) :- !. % See rdf_template
user:goal_expansion(inner::G, call_inner(G)) :- !. % See rdf_template
user:goal_expansion(::G, particle:particle_self(G)) :-
	loading_particle(_, _).
	
%	::(+Particle, +Goal)
%	
%	Call Goal in particle.  The reason for its existence is to keep
%	track of `self'.

::(Name, Goal) :-
	Name:Goal,
	true.				% avoid last call optimization

particle_self(Goal) :-
	(   prolog_current_frame(Frame),
	    prolog_frame_attribute(Frame, parent_goal, % SWI-Prolog > 5.2.9
				   particle:(::(Particle, _)))
	->  Particle:Goal
	;   throw(error(existence_error(particle, self), _))
	).

		 /*******************************
		 *	  ADMINISTRATION	*
		 *******************************/

%	current_particle(?Name)
%	
%	Test/enumerate defined particles.

current_particle(Name) :-
	current_particle(Name, _Supers).


		 /*******************************
		 *	       PceEmacs		*
		 *******************************/

:- multifile
	emacs_prolog_colours:term_colours/2,
	emacs_prolog_colours:goal_classification/2.

emacs_prolog_colours:term_colours((:- begin_particle(_, _)),
	     expanded - [ expanded - [ identifier,
				       classify
				     ]
			]).
emacs_prolog_colours:term_colours((:- end_particle),
	     expanded - [ expanded
			]).
emacs_prolog_colours:term_colours(:- end_particle(_),
	     expanded - [ expanded - [ identifier
				     ]
			]).
