/*  File:    particle.pl
    Author:  Jan Wielemaker
    Created: Jun 16 2003
    Purpose: Very minimalisatic object system
*/

:- module(particle,
	  [ begin_particle/2,		% +Name, +Supers
	    end_particle/0,
	    current_particle/1,		% ?Particle
	    (::)/2			% +Particle, +Goal
	  ]).

:- op(600, xfy, user:(::)).
:- op(600, fy,  user:(::)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Extremly lightweight package to do  use   modules  more like classes. It
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

begin_particle(Name, Super0) :-
	canonical_super(Super0, Supers),
	(   current_particle(Name, Supers)
	->  true
	;   retractall(current_particle(Name, _)),
	    unimport(Name),
	    assert(current_particle(Name, Supers)),
	    set_import_modules(Name, Supers)
	),
	'$set_source_module'(Old, Name),
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
	forall(import_module(Module, I), writeln(I)),
	forall(member(I, Imports), add_import_module(Module, I, end)),
	'$set_source_module'(Context, Context),
	add_import_module(Module, Context, end).

%	unimport(+Module)
%	
%	Remove all predicates that have been auto-imported, so the
%	links can be restored from the proper module on the next call.

unimport(Module) :-
	(   predicate_property(M:Head, imported_from(I)),
	    import_module(Module, I),
	    functor(Head, Name, Arity),
	    abolish(M:Name/Arity),
	    fail
	;   true
	).


		 /*******************************
		 *	     SUPER/SELF		*
		 *******************************/

:- multifile
	user:term_expansion/2,
	user:goal_expansion/2.

user:term_expansion((:- end_particle),
		    (:- particle:end_particle)).

user:goal_expansion(super::G, S:G) :-
	(   loading_particle(L, _)
	->  current_particle(L, Supers),
	    (   Supers = [S]
	    ->	true
	    ;	throw(error(ambiguous(super, Supers), _))
	    )
	).
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
	    find_self_particle(Frame, Particle)
	->  Particle:Goal
	;   throw(error(existence_error(particle, self), _))
	).

find_self_particle(Frame, Particle) :-
	prolog_frame_attribute(Frame, goal, Goal),
	goal_particle(Goal, Particle), !.
find_self_particle(Frame, Particle) :-
	prolog_frame_attribute(Frame, parent, Parent),
	find_self_particle(Parent, Particle).
	
goal_particle(_:(::(Particle, _Goal)), Particle).


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
