/*  File:    rdfs_hierarchy.pl
    Author:  Jan Wielemaker
    Created: Jun 17 2003
    Purpose: 
*/

:- module(rdfs_hierarchy, []).
:- use_module(library(pce)).
:- use_module(rdf_tree).

:- pce_begin_class(rdfs_hierarchy, rdf_tree,
		   "Tmporary stuff").

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

find_from(OT, String:name, How:[name], Fields:[chain], Max:[int], ReportTo:[object]) :->
	statistics(cputime, CPU0),
	default(How, substring, TheHow),
	default(Max, 100, MaxCount),
	send(OT, selection, @nil),
	get(OT, domain, Domain),
	(   Fields == @default
	->  PlFields = [rdfs:label]
	;   chain_list(Fields, PlFields)
	),
	new(Hits, hash_table),
	(   rdfs_find(String, Domain, PlFields, TheHow, Subject),
	    \+ get(Hits, member, Subject),
	    send(Hits, append, Subject),
	    send(OT, show_hit, Subject),
	    get(Hits, size, Count),
	    (   Count > MaxCount
	    ->  true
	    ;   send(ReportTo, report, progress, 'Found %d ...', Count),
	        fail
	    )
	;   true
	),
	get(Hits, size, Count),
	(   Count == 0
	->  send(ReportTo, report, warning, 'No hits'),
	    send(OT, expand_domain)
	;   Count =< MaxCount
	->  statistics(cputime, CPU1),
	    CPU is CPU1 - CPU0,
	    send(ReportTo, report, done, 'completed in %.2f seconds', CPU)
	;   send(ReportTo, report, status, 'Shown first %d hits', MaxCount)
	).

show_hit(OT, Id:name) :->
	"Show hit of search"::
	get(OT, add, Id, Node),
	send(Node, selected, @on).

:- pce_end_class(rdfs_hierarchy).
