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

%	call_rules(+Obj, +Goal)
%	
%	Search the container classes for the first matching container
%	defining Goal and call it.

call_rules(Obj, Goal) :-
	container_with_particle(Obj, Particle),
	current_predicate(_, Particle:Goal),
	Particle::Goal.

container_with_particle(Obj, Particle) :-
	get(Obj, class_name, Particle),
	current_particle(Particle).
container_with_particle(Obj, Particle) :-
	(   get(Obj, contained_in, Container)
	->  container_with_particle(Container, Particle)
	;   get(Obj, create_context,
		message(@arg1, instance_of, visual),
		Context)
	->  writeln(Context),
	    container_with_particle(Context, Particle)
	;   print_message(error, not_contained(Obj)),
	    fail
	).


:- multifile
	prolog:message/3.

prolog:message(not_contained(Obj)) -->
	[ 'Object ~p has no container'-[Obj] ].
