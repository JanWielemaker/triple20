/*  File:    rdf_template.pl
    Author:  Jan Wielemaker
    Created: Feb 27 2003
    Purpose: Define common template functionality
*/


:- module(rdf_template, []).
:- use_module(library(pce)).
:- use_module(library(pce_template)).

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

node_label(T, Id:name, Label:name) :<-
	"Get label to display for Id"::
	(   get(T, show_namespace, @off)
	->  rdfs_label(Id, Label)
	;   rdfs_ns_label(Id, Label)
	).

:- pce_end_class(rdf_container).


		 /*******************************
		 *	       UTIL		*
		 *******************************/

container_with_send_method(Gr, Method, Gr) :-
	send(Gr, has_send_method, Method).
container_with_send_method(Gr, Method, Container) :-
	get(Gr, contained_in, Container0),
	container_with_send_method(Container0, Method, Container).
