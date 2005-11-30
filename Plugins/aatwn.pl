:- module(t20aatwn, []).
:- include(triple20(plugin)).
:- plugin([ rdfs:label   = 'AAT-WordNet',
	    rdfs:comment = 'Relate AAT and WordNet'
	  ]).

:- use_module(t20plugin(wordnet)).
:- use_module(t20plugin(skos)).

:- begin_rules(rdf_node, t20aatwn_rules).

%	drop_resource_command(+Onto, +Drop, -Command)
%	
%	Determine the command(s) to execute if an object representing Drop
%	is dropped onto an object representing Ondo.  

					% drop AAT on WordNet
drop_resource_command(C, R, Command) :-
	rdfs_individual_of(C, wn2:'Synset'),
	rdfs_individual_of(R, aat:'Subject').
drop_resource_command(C, R, Command) :-
	super::drop_resource_command(C, R, Command).

relation(rdfs:subClassOf).
relation(rdf:type).
relation(owl:sameAs).
relation(skos:broader).
relation(skos:broaderInstantive).
relation(skos:broaderPartitive).
relation(skos:broaderGeneric).

%	drop_resource(+Command, +Ondo, +Drop)
%	
%	Perform the rdf modifications for the   given Command if Drop is
%	dropped Onto.

drop_resource(skos_narrower, C, R) :- !,			% drop R on C
	rdf_add_object(R, skos:narrower, C).
drop_resource(Command, C, R) :-
	super::drop_resource(Command, C, R).

:- end_rules.
