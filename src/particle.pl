/*  File:    particle.pl
    Author:  Jan Wielemaker
    Created: Jun 16 2003
    Purpose: Very minimalisatic object system
*/

:- module(particle,
	  [ begin_particle/2,
	    end_particle/0
	  ]).

:- dynamic
	loading_particle/2,
	current_particle/2.		% defined particles.

begin_particle(Name, Super) :-
	(   current_particle(Name, Super)
	->  true
	;   retractall(current_particle(Name, _)),
	    assert(current_particle(Name, Super))
	),
	'$set_source_module'(Old, Name),
	asserta(loading_particle(Name, Old)),
	(   Super \== []
	->  '$default_module'(Name, _, Super)
	;   true
	).

end_particle :-
	retract(loading_particle(_, Old)), !,
	'$set_source_module'(_, Old).

:- multifile
	user:term_expansion/2,
	user:goal_expansion/2.

user:term_expansion((:- end_particle),
		    (:- particle:end_particle)).

user:goal_expansion(super:G, S:G) :-
	(   loading_particle(L, _)
	->  current_particle(L, S)
	).
	
