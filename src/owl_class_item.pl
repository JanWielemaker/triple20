/*  File:    owl_class_item.pl
    Author:  Jan Wielemaker
    Created: Mar 10 2003
    Purpose: Visualisations of an owl class
*/

:- module(owl_class_item, []).
:- use_module(library(pce)).
:- use_module(library(rdf_db)).
:- use_module(library(rdfs)).


:- pce_begin_class(owl_class_item, device,
		   "Visualise an OWL class").

variable(type,	{union_of,intersection_of,complement_of,one_of,simple},
	 get, "Operator type").

initialise(I, Description:name) :->
	"Create from resource"::
	(   rdf_has(Description, owl:unionOf, Set)
	->  send(I, type, union_of),
	    forall(rdfs_member(Sub, Set),
		   send(I, append, Sub))
	;   rdf_has(Description, owl:intersectionOf, Set)
	->  send(I, type, intersection_of),
	    forall(rdfs_member(Sub, Set),
		   send(I, append, Sub))
	;   rdf_has(Description, owl:complementOf, Arg)
	->  send(I, type, complement_of),
	    send(I, append, Arg)
	;   send(I, type, simple),
	    send(I, append, Description)
	).

append(I, Resource:name) :->
	get(I, type, Type),
	(   (   Type == union_of
	    ;	Type == intersection_of
	    )
	->  send(I, separator),
	    send(I, display, owl_class_item(Resource))
	;   Type == complement_of
	->  send(I, display, owl_class_item(Resource))
	;   Type == simple
	->  send(I, display, rdf_resource_text(Resource, I))
	).

seperator(I) :->
	"Append a separator"::
	(   get(I?graphicals, size, 0)
	->  true
	;   get(I, separator, Gr),
	    send(I, display, Gr)
	).

separator(I, Gr:graphical) :<-
	(   get(I, type, union_of)
	->  symbol(200, Gr)
	;   get(I, type, intersection_of)
	->  symbol(199, Gr)
	).

symbol(N, Gr) :-
	new(Gr, text(string('%c', N),
		     font(symbol, roman, 24))).

type(I, Type:{union_of,intersection_of,complement_of,one_of,simple}) :->
	send(I, slot, type, Type),
	(   (   Type == union_of
	    ;	Type == intersection_of
	    )
	->  new(Fmt, format(horizontal, 1, @on)),
	    send(I, format, Format)
	;   Type == complement_of
	->  new(Fmt, format(vertical, 1, @on)),
	    send(I, format, Format),
	    send(I, display, new(line))
	;   send(I, format, @nil)
	).

:- pce_end_class(owl_class_item).
