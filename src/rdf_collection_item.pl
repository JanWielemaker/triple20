/*  File:    rdfs_collection_item.pl
    Author:  Jan Wielemaker
    Created: Feb 26 2003
    Purpose: Visualize collections
*/


:- module(rdfs_collection_item, []).
:- use_module(library(pce)).
:- use_module(library(broadcast)).
:- use_module(semweb(rdf_db)).
:- use_module(library(pce_graphical_browser)).
:- use_module(library(rdf_template)).
	  
:- pce_begin_class(rdf_list_browser, graphical_browser,
		   "Browse a collection as a list").
:- use_class_template(rdf_container).

variable(resource, name*,      get, "Resource describing the list").
variable(auto_fit, int* := 5,  get, "AUtomatically re-fit after ->update").
variable(rules,    name := rdf_list_rules, both, "Rules for plugins").

initialise(LB, Collection:[name]*) :->
	send_super(LB, initialise),
	send(LB, single_column, @on),
	send(LB, gap, size(5,0)),	% no distance between lines
	(   atom(Collection)
	->  send(LB, resource, Collection)
	;   true
	).

resource(LB, Resource:name*) :->
	"Associate a list resource"::
	(   get(LB, resource, Resource)
	->  true
	;   send(LB, slot, resource, Resource),
	    send(LB, update)
	).

update(LB) :->
	"Re-load the set"::
	send(LB, clear),
	get(LB, resource, Collection),
	send(LB, append_list, Collection),
	(   get(LB, auto_fit, Lines),
	    Lines \== @nil
	->  send(LB, fit, Lines)
	;   true
	).

append_list(LB, Collection:name) :->
	"Append object part of a triple"::
	(   rdf_equal(Collection, rdf:nil)
	->  true
	;   rdf_equal(rdf:first, First),
	    rdf_has(Collection, First, Element),
	    get(LB, rules, RuleSet),
	    RuleSet:collection_item_class(rdf(Collection, First, Element),
					  LB,
					  Class),
	    NewTerm =.. [Class, Collection, First, Element, LB],
	    new(Gr, NewTerm),
	    send(LB, append, Gr),
	    rdf_has(Collection, rdf:rest, Rest),
	    send(LB, append_list, Rest)
	).

fit(LB, MaxSize:[int]) :->
	"Adjust the size for small sets"::
	default(MaxSize, 5, Max),
	get(LB?graphicals, size, SetSize),
	(   SetSize =< Max
	->  send(LB, scroll_to, point(0,0)),
	    get(LB, bounding_box, area(_,_,W,H)),
	    WH is max(16, H),		% 1 line in current font?
	    send(LB, size, size(W, WH))
	;   H is Max*20,
	    send(LB, height, H)
	).

:- pce_end_class(rdf_list_browser).


		 /*******************************
		 *     BROWSE OBJECT FIELD	*
		 *******************************/

:- pce_begin_class(rdf_object_list_browser, rdf_list_browser,
		   "Represent object part of a triple").

variable(subject,   name*, get, "RDF subject").
variable(predicate, name*, get, "RDF predicate").

initialise(T,
	   Subject:subject=name,
	   Predicate:predicate=name,
	   Value:object=name,		% collection resource
	   Container:container=[object]) :->
	send(T, slot, subject, Subject),
	send(T, slot, predicate, Predicate),
	send_super(T, initialise, Value),
	(   get(Container?window, background, BG)
	->  send(T, background, BG),
	    send(T, pen, 0)		% Make myself invisible
	;   true
	).

:- pce_end_class(rdf_object_list_browser).
