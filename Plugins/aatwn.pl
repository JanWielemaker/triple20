:- module(t20aatwn, []).
:- use_module(triple20(rdf_rules)).
:- use_module(triple20(rdf_util)).
:- use_module(library('semweb/rdfs')).
:- use_module(library('semweb/rdf_db')).

:- begin_rules(rdf_node, aatwn).

%	drop_resource_command(+Onto, +Drop, -Command)
%	
%	Determine the command(s) to execute if an object representing Drop
%	is dropped onto an object representing Ondo.  

drop_resource_command(C, R, skos_narrower) :-
	rdfs_individual_of(C, rdf:'Property'),
	rdfs_individual_of(R, rdf:'Property').
drop_resource_command(C, R, Command) :-
	super::drop_resource_command(C, R, Command).

%	drop_resource(+Command, +Ondo, +Drop)
%	
%	Perform the rdf modifications for the   given Command if Drop is
%	dropped Onto.

drop_resource(skos_narrower, C, R) :- !,			% drop R on C
	rdf_add_object(R, skos:narrower, C).
drop_resource(Command, C, R) :-
	super::drop_resource(Command, C, R).

:- end_rules.
