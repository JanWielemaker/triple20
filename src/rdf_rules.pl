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

:- module(rdf_rules,
	  [ begin_rules/2,		% +Class, +Topic
	    end_rules/0,

	    call_rules/2,		% +Object, :Goal
	    call_outer/1,		% :Goal
	    call_inner/1,		% :Goal

	    rdf_user_call/1,		% :Goal
	    rdf_user_call/2		% +Graphical, :Goal
	  ]).
:- use_module(library(pce)).
:- use_module(library(debug)).
:- use_module(particle).


%	user:goal_expansion(+NSGoal, -Goal)
%	
%	This predicate allows for writing down rdf queries in a friendly
%	name-space fashion.  

:- multifile
	user:goal_expansion/2.

user:goal_expansion(rdf_user_call(G0),
		    rdf_user_call(G)) :-
	expand_goal(G0, G).
user:goal_expansion(rdf_user_call(Gr, G0),
		    rdf_user_call(Gr, G)) :-
	expand_goal(G0, G).


		 /*******************************
		 *	ASSOCIATE TO CLASS	*
		 *******************************/

%	:- begin_rules(+Class, +Id).
%	:- end_rules.
%	
%	Associate a ruleset with  Class  with   the  given  id. If Class
%	already has a ruleset attached  a   new  ruleset is created that
%	subsumes the current one and the class  is associated to the new
%	ruleset.

:- dynamic
	refined_particle/4.		% Class, For, Particle, Super

begin_rules(Class, For) :-
	refined_particle(Class, For, Particle, Super), !,
	source_location(File, Line),
	begin_particle(Particle, Super, File:Line).
begin_rules(ClassName, For) :-
	get(@pce, convert, ClassName, class, Class),
	get(Class, rdf_particle, Particle), !,
	debug(rules,
	      'Class ~w already has particle ~w~n',
	      [ClassName, Particle]),
	concat_atom([For, Particle], '_', Refined),
	send(Class, rdf_particle, Refined),
	assert(refined_particle(ClassName, For, Refined, Particle)),
	source_location(File, Line),
	begin_particle(Refined, Particle, File:Line).
begin_rules(Class, For) :-
	assert(refined_particle(Class, For, Class, [])),
	source_location(File, Line),
	begin_particle(Class, [], File:Line).

end_rules :-
	end_particle.

user:term_expansion(:- end_rules,
		    :- rdf_rules:end_rules).


		 /*******************************
		 *	      PARTICLE		*
		 *******************************/

:- dynamic
	class_particle/3.		% Class, Particle, implicit/explicit

update_class_particle(ClassName, Particle) :-
	isa_class(ClassName, Particle),
	current_particle(Particle), !,
	assert(class_particle(ClassName, Particle, implicit)).
update_class_particle(ClassName, _) :-
	assert(class_particle(ClassName, [], implicit)),
	fail.

:- multifile
	user:message_hook/3.
:- dynamic
	user:message_hook/3.

user:message_hook(load_file(_), _, _) :-
	retractall(class_particle(_,_, implicit)),
	fail.

:- pce_extend_class(object).

rdf_particle(O, Particle:name) :<-
	get(O, class_name, ClassName),
	(   class_particle(ClassName, Particle, _)
	->  Particle \== []
	;   update_class_particle(ClassName, Particle)
	).

:- pce_end_class.

:- pce_extend_class(class).

rdf_particle(Class, Particle:name) :<-
	get(Class, name, ClassName),
	(   class_particle(ClassName, Particle, _)
	->  Particle \== []
	;   update_class_particle(ClassName, Particle)
	).

rdf_particle(Class, Particle:name) :->
	"Bind to a particle"::
	get(Class, name, ClassName),
	retractall(class_particle(ClassName, _, _)),
	assert(class_particle(ClassName, Particle, explicit)).

:- pce_end_class.

:- pce_extend_class(visual).

rdf_container(O, Container:visual) :<-
	get(O, contained_in, Container).

:- pce_end_class.

:- pce_extend_class(graphical).

rdf_container(Gr, Container:object) :<-
	(   get(Gr, layout_interface, Container),
	    Container \== @nil
	->  true
	;   get(Gr, contained_in, Container)
	).

:- pce_end_class.

:- pce_extend_class(layout_interface).

rdf_container(O, Container:layout_manager) :<-
	get(O, layout_manager, Container),
	Container \== @nil.

:- pce_end_class.

:- pce_extend_class(layout_manager).

rdf_container(O, Container:device) :<-
	get(O, device, Container),
	Container \== @nil.

:- pce_end_class.



		 /*******************************
		 *	     CALL RULES		*
		 *******************************/

:- pce_global(@particle, new(?(@prolog, current_container))).

%	call_rules(+Obj, +Goal)
%	
%	Search the container classes for the first matching container
%	defining Goal and call it.

call_rules(Obj, Goal) :-
	container_with_particle_defining(Obj, Goal, Container, Particle),
	with_container(Container, Particle::Goal),
	true.				% avoid tail recursion

container_with_particle_defining(Obj, Goal, Container, Particle) :-
	debug(container,
	      'Searching container of ~p to call ~p', [Obj, Goal]),
	container_with_particle(Obj, Container, Particle),
	defined(Particle:Goal), !,
	debug(container,
	      '~p: calling ~w::~p~n', [Obj, Particle, Goal]).

%	defined(:G)
%	
%	True  if  G  is  defined.  We  use  '$c_current_predicate'/2  as
%	current_predicate/2 updates the foreign  library   index  and is
%	therefore much too slow.

defined(M:G) :-
	default_module(M, S),
	'$c_current_predicate'(_, S:G), !.

%	call_outer(+Goal)
%	
%	Called from a ruleset to invoke  rules of the outer environment.
%	Where  super::Goal  walks   up    the   inheritance   hierarchy,
%	outer::Goal walks the contained-in hierarchy.

call_outer(Goal) :-
	current_container(C0),
	container(C0, C1),
	container_with_particle(C1, Container, Particle),
	defined(Particle:Goal), !,
	with_container(Container, Particle::Goal).

with_container(_Container, Goal) :-
	Goal,
	true.				% avoid last-call optimisation

current_container(Cont) :-
	(   prolog_current_frame(Frame),
	    prolog_frame_attribute(Frame, parent_goal,
				   rdf_rules:with_container(Cont, _))
	->  true
	;   throw(error(existence_error(container, current), _))
	).

%	call_inner(+Goal)
%	
%	Calls Goal starting from the same place as the original
%	call_rules.

call_inner(Goal) :-
	current_inner(Obj),
	container_with_particle_defining(Obj, Goal, Container, Particle),
	with_container(Container, Particle::Goal).

current_inner(Obj) :-
	(   prolog_current_frame(Frame),
	    prolog_frame_attribute(Frame, parent_goal,
				   rdf_rules:call_rules(Obj, _))
	->  true
	;   gtrace, current_inner(Obj)
	    %throw(error(existence_error(container, inner), _))
	).


container_with_particle(Obj, Obj, Particle) :-
	get(Obj, rdf_particle, Particle),
	current_particle(Particle),
	debug(container, '~p has particle ~p', [Obj, Particle]).
container_with_particle(Obj, Container, Particle) :-
	container(Obj, Container0),
	container_with_particle(Container0, Container, Particle).

%	container(+Object, -Container)
%	
%	Find container (context) in which Object   exists. If the object
%	is being created, <-create_context  finds   the  receiver of the
%	XPCE method in which the initialisation   method is executed or,
%	in other words, the context that is creating us.

container(Obj, Container) :-
	(   get(Obj, rdf_container, Container)
	->  debug(container,
		  'Try container of ~p: ~p', [Obj, Container])
	;   get(Obj, create_context,
		message(@arg1, has_get_method, rdf_container),
		Container)
	->  debug(container,
		  'Try create context of ~p: ~p~n', [Obj, Container])
	;   print_message(error, not_contained(Obj)),
%	    trace,
	    fail
	).


		 /*******************************
		 *	  GUARDED CALLING	*
		 *******************************/

:- meta_predicate
	rdf_user_call(:),
	rdf_user_call(+, :).

%	rdf_user_call(:Goal)
%	
%	Run goal, report messages in the status window.

rdf_user_call(Goal) :-
	rdf_user_call(@pce, Goal).

rdf_user_call(Gr, Goal) :-
	catch(Goal, E, rdf_rules:report_exception(Gr, E)).

report_exception(Gr, E) :-
	message_to_string(E, Message),
	send(Gr, report, error, Message).


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(not_contained(Obj)) -->
	[ 'Object ~p has no container'-[Obj] ].
