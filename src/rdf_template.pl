/*  File:    rdf_template.pl
    Author:  Jan Wielemaker
    Created: Feb 27 2003
    Purpose: Define common template functionality
*/


:- module(rdf_template,
	  [ call_rules/2,
	    container_with_get_method/3, % +Gr, +Method, -Container
	    container_with_send_method/3
	  ]).
:- use_module(library(pce)).
:- use_module(library(pce_template)).
:- use_module(particle).
:- use_module(library(debug)).

:- pce_autoload(rdf_explorer,  rdf_explorer).

:- pce_begin_class(rdf_arm, template,
		   "(Un)arm objects in a window").

:- pce_group(event).

define_event(Name, Parent) :-
	(   get(@event_tree, node, Name, Node)
	->  (   get(Node?parent, value, Parent)
	    ->	true
	    ;	print_message(error, format('Redefined event ~w', [Name]))
	    )
	;   new(_, event_node(Name, Parent))
	).

:- pce_global(@arm_recogniser,
	      new(handler(arm,
			  message(@event?display, try_arm,
				  @event?receiver)))).
:- initialization
   define_event(arm, user).

event(W, Ev:event) :->
	(   send(Ev, is_a, loc_move),
	    get(W, arm, _Target)
	->  true
	;   send(Ev, is_a, area_exit)
	->  send(@display, arm_object, @nil)
	;   true
	),
	send_super(W, event, Ev).

:- pce_group(arm).

arm(W, For:[name], Target:graphical) :<-
	(   get(@event, position, W, point(X, Y)),
	    get(W, slot, scroll_offset, point(OX, OY)),
	    AX is X + OX, AY is Y+OY,
	    new(Ev, event(arm, W, AX, AY)),
	    (	For == @default
	    ->	true
	    ;	send(Ev, attribute, arm_for, For)
	    ),
	    debug(arm, 'Posting arm to ~p at ~d,~d', [W, AX, AY]),
	    send(Ev, post, W)
	->  get(@display, hypered, arm, Target)
	;   send(@display, arm_object, @nil),
	    fail
	).
	
:- pce_end_class(rdf_arm).


:- pce_extend_class(display).

try_arm(W, Gr:graphical) :->
	(   get(@event, attribute, arm_for, For)
	->  send(Gr, has_send_method, For)
	;   true
	),
	send(W, arm_object, Gr).

arm_object(W, Gr:graphical*) :->
	(   get(W, hypered, arm, Old)
	->  (   Old == Gr
	    ->	true			% no change
	    ;	send(W, delete_hypers, arm),
		send(Old, arm, @off)
	    )
	;   true
	),
	(   Gr \== @nil
	->  send(Gr, arm, @on),
	    new(_, hyper(W, Gr, arm, arm_window))
	;   true
	).

:- pce_end_class(display).


:- pce_begin_class(rdf_container, template,
		   "Common behaviour for containers").

:- pce_group(namespace).

variable(show_namespace, bool := @on,  get, "Do (not) show namespace").

:- pce_end_class(rdf_container).


:- pce_begin_class(rdf_resource_template, template,
		   "RDF visual providing <-resource").

:- pce_group(edit).

show_details(T, How:{hierarchy,table}) :->
	"Show details in format"::
	(   get(T, frame, Frame),
	    send(Frame, has_send_method, show_resource)
	->  get(T, resource, Resource),
	    send(Frame, show_resource, Resource, How)
	).

hierarchy_location(T) :->
	send(T, show_details, hierarchy).
details(T) :->
	send(T, show_details, table).

can_show_details(T, _How:{hierarchy,table}) :->
	"Test if we are embedded in a context that can show details"::
	get(T, frame, Frame),
	send(Frame, has_send_method, show_resource).


view_rdf_source(T) :->
	"Open Prolog editor on RDF source"::
	get(T, resource, Id),
	(   rdf_source_location(Id, File:Line)
	->  edit(file(File, line(Line)))
	;   send(T, report, warning, 'Cannot find source for %s', Id)
	).

copy(T, As:[{resource,xml_identifier,xml_attribute}]) :->
	"Copy resource to clipboard"::
	get(T, resource, Resource),
	(   As == xml_identifier
	->  rdf_global_id(NS:Local, Resource),
	    new(Copy, string('%s:%s', NS, Local))
	;   As == xml_attribute
	->  rdf_global_id(NS:Local, Resource),
	    new(Copy, string('&%s;%s', NS, Local))
	;   Copy = Resource
	),
	send(@display, copy, Copy).

copy_id(T)                :-> send(T, copy, resource).
copy_as_xml_identifier(T) :-> send(T, copy, xml_identifier).
copy_as_xml_attribute(T)  :-> send(T, copy, xml_attribute).


:- pce_group(diagram).

diagram_(T) :->
	"Open triple diagram from resource Id"::
	get(T, resource, Resource),
	get(T, rdf_diagram, Diagram),
	send(Diagram, resource, Resource),
	send(Diagram, expose).

rdf_diagram(T, Diagram:rdf_explorer) :<-
	"Get associated RDF explorer"::
	get(T, frame, Frame),
	(   get(Frame, hypered, rdf_explorer, Diagram)
	->  true
	;   new(Diagram, rdf_explorer),
	    new(_, partof_hyper(Frame, Diagram, rdf_explorer, hierarchy))
	).

:- pce_group(drag_and_drop).

preview_drop(T, Resource:name*) :->
	(   Resource == @nil
	->  send(T, report, status, '')
	;   call_rules(T, drop_command(T, Resource, Cmd)),
	    get(T, resource, OnTo),
	    rdfs_ns_label(Resource, RL),
	    rdfs_ns_label(OnTo, OntoLabel),
	    send(T, report, status,
		 'Drop %s onto %s: %s', RL, OntoLabel, Cmd?label_name)
	).

drop(T, Resource:name) :->
	call_rules(T, drop(T, Resource)).

on_left_click(V) :->
	"Left-click on object: find container that deals with it"::
	send(@display, busy_cursor),
	call_cleanup(call_rules(V, clicked(V)),
		     send(@display, busy_cursor, @nil)).

:- pce_group(rdf).

triple(T, Value:prolog) :<-
	"Find part the triple I belong to"::
	get(T, contained_in, C0),
	container_with_get_method(C0, triple_from_part, Container),
	get(Container, triple_from_part, T, Value).

subject(T, Subject:name) :<-
	get(T, triple, rdf(Subject, _, _)).

predicate(T, Predicate:name) :<-
	get(T, triple, rdf(_, Predicate, _)).

object(T, Object:prolog) :<-
	get(T, triple, rdf(_, _, Object)).

:- pce_end_class(rdf_resource_template).




		 /*******************************
		 *	       UTIL		*
		 *******************************/

container_with_send_method(Gr, Method, Gr) :-
	send(Gr, has_send_method, Method).
container_with_send_method(Gr, Method, Container) :-
	get(Gr, contained_in, Container0),
	container_with_send_method(Container0, Method, Container).


container_with_get_method(Gr, Method, Gr) :-
	send(Gr, has_get_method, Method).
container_with_get_method(Gr, Method, Container) :-
	get(Gr, contained_in, Container0),
	container_with_get_method(Container0, Method, Container).


		 /*******************************
		 *	      PARTICLE		*
		 *******************************/

:- pce_extend_class(object).

rdf_particle(O, Particle:name) :<-
	get(O, class_name, Particle).

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

:- pce_extend_class(node).

rdf_container(N, Container:visual) :<-
	get(N, tree, Container),
	Container \== @nil.

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
	current_particle(Particle),
	debug(container, '~p has particle ~p', [Obj, Particle]).
container_with_particle(Obj, Particle) :-
	(   get(Obj, rdf_container, Container)
	->  debug(container, 'Try container of ~p: ~p', [Obj, Container]),
	    container_with_particle(Container, Particle)
	;   get(Obj, create_context,
		message(@arg1, instance_of, visual),
		Context)
	->  debug(container, 'Try create context of ~p: ~p~n', [Obj, Context]),
	    container_with_particle(Context, Particle)
	;   print_message(error, not_contained(Obj)),
%	    trace,
	    fail
	).


:- multifile
	prolog:message/3.

prolog:message(not_contained(Obj)) -->
	[ 'Object ~p has no container'-[Obj] ].
