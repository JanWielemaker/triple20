/*  File:    rdf_template.pl
    Author:  Jan Wielemaker
    Created: Feb 27 2003
    Purpose: Define common template functionality
*/


:- module(rdf_template, []).
:- use_module(library(pce)).
:- use_module(library(pce_template)).

:- pce_begin_class(rdf_container, template,
		   "Common behaviour for containers").

:- pce_group(namespace).

variable(show_namespace, bool := @on,  get, "Do (not) show namespace").

node_label(T, Id:name, Label:name) :<-
	"Get label to display for Id"::
	(   get(T, show_namespace, @off)
	->  rdfs_label(Id, Label)
	;   rdfs_ns_label(Id, Label)
	).

:- pce_end_class(rdf_container).
