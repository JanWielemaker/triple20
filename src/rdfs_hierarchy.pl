/*  File:    rdfs_hierarchy.pl
    Author:  Jan Wielemaker
    Created: Jun 17 2003
    Purpose: 
*/

:- module(rdf_hierarchy, []).
:- use_module(library(pce)).
:- use_module(rdf_tree).

:- pce_begin_class(rdfs_hierarchy, rdf_tree,
		   "Temporary stuff").

variable(domain,	prolog, get, "Domain of visualised objects").

initialise(H, Domain:[prolog]) :->
	send_super(H, initialise),
	(   Domain \== @default
	->  send(H, slot, domain, Domain)
	;   rdf_equal(D, rdfs:'Resource'),
	    send(H, slot, domain, D)
	).

expand_domain(H) :->
	send(H, expand_root).

collapse_domain(H) :->
	send(H?root, collapsed, @on).

:- pce_group(search).

find(OT) :->
	"Start interactive find"::
	new(D, dialog('Search ontology')),
	send(D, append, new(TI, text_item(find))),
	send(D, append,
	     new(Find, button(find,
			      and(message(D, report, progress, 'Searching ...'),
				  message(OT, find_from, TI?selection, D),
				  message(D, destroy))))),
	send(D, append, button(cancel, message(D, destroy))),
	send(D, append, new(reporter)),
	send(D, resize_message, message(D, layout, size := @arg2)),
	send(Find, default_button, @on),
	send(Find, active, @off),
	send(D, transient_for, OT?frame),
	send(D, open_centered, ?(@event, position, @display)).

find_from(OT, String:for=name, How:how=[name],
         Fields:predicates=[chain], Max:max=[int]) :->
	"Compatibility"::
	send_super(OT, find, String, How, Fields, Max).

:- pce_end_class(rdfs_hierarchy).
