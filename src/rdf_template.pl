/*  File:    rdf_template.pl
    Author:  Jan Wielemaker
    Created: Feb 27 2003
    Purpose: Define common template functionality
*/


:- module(rdf_template,
	  [ call_rules/2
	  ]).
:- use_module(library(pce)).
:- use_module(library(pce_template)).
:- use_module(particle).
:- use_module(library(debug)).

:- pce_begin_class(rdf_arm, template,
		   "(Un)arm objects in a window").

:- pce_group(event).

event(W, Ev:event) :->
	(   get(W, focus, Gr),
	    Gr \== @nil
	->  true
	;   send(Ev, is_a, loc_move)
	->  send(W, check_arm, Ev)
	;   send(Ev, is_a, area_exit)
	->  send(W, arm_object, @nil)
	;   true
	),
	send_super(W, event, Ev).

:- pce_group(arm).

check_arm(W, Ev:event) :->
	(   get(W, find, Ev,
		message(@arg1, has_send_method, arm),
		Gr)
	->  send(W, arm_object, Gr)
	;   send(W, arm_object, @nil)
	).

arm_object(W, Gr:graphical*) :->
	(   get(W, hypered, arm, Old)
	->  send(W, delete_hypers, arm),
	    send(Old, arm, @off)
	;   true
	),
	(   Gr \== @nil
	->  send(Gr, arm, @on),
	    new(_, hyper(W, Gr, arm, arm_window))
	;   true
	).

:- pce_end_class(rdf_arm).


:- pce_begin_class(rdf_visual, template,
		   "Common behaviour to all RDF visualisers").

on_left_click(V) :->
	"Left-click on object: find container that deals with it"::
	container_with_send_method(V, clicked, Container), !,
	send(Container, clicked, V).

:- pce_end_class(rdf_visual).


:- pce_begin_class(rdf_container, template,
		   "Common behaviour for containers").
:- use_class_template(rdf_visual).

:- pce_group(namespace).

variable(show_namespace, bool := @on,  get, "Do (not) show namespace").

:- pce_end_class(rdf_container).


		 /*******************************
		 *	       UTIL		*
		 *******************************/

container_with_send_method(Gr, Method, Gr) :-
	send(Gr, has_send_method, Method).
container_with_send_method(Gr, Method, Container) :-
	get(Gr, contained_in, Container0),
	container_with_send_method(Container0, Method, Container).


		 /*******************************
		 *	      PARTICLE		*
		 *******************************/

:- pce_extend_class(object).

rdf_particle(O, Particle:name) :<-
	get(O, class_name, Particle).

rdf_container(O, Container:visual) :<-
	get(O, contained_in, Container).

:- pce_end_class.

:- pce_extend_class(node).

rdf_container(N, Container:visual) :<-
	(   get(N, parents, Parents),
	    Parents \== @nil,
	    get(Parents, head, Container)
	->  true
	;   get(N, tree, Container),
	    Container \== @nil
	).

:- pce_end_class.


		 /*******************************
		 *	       RULES		*
		 *******************************/

%	call_rules(+Obj, +Goal)
%	
%	Search the container classes for the first matching container
%	defining Goal and call it.

call_rules(Obj, Goal) :-
	container_with_particle(Obj, Particle),
	current_predicate(_, Particle:Goal), !,
	Particle::Goal.

container_with_particle(Obj, Particle) :-
	get(Obj, rdf_particle, Particle),
	current_particle(Particle).
container_with_particle(Obj, Particle) :-
	(   get(Obj, rdf_container, Container)
	->  container_with_particle(Container, Particle)
	;   get(Obj, create_context,
		message(@arg1, instance_of, visual),
		Context)
	->  debug(container, '~p~n', [Context]),
	    container_with_particle(Context, Particle)
	;   print_message(error, not_contained(Obj)),
	    fail
	).


:- multifile
	prolog:message/3.

prolog:message(not_contained(Obj)) -->
	[ 'Object ~p has no container'-[Obj] ].
