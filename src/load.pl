/*  File:    load.pl
    Author:  Jan Wielemaker
    Created: Jun 25 2003
    Purpose: Load demo version of the tool
*/

user:file_search_path(semweb, library(semweb)).
:- asserta(file_search_path(library, '.')).

:- dynamic
	rdf_db:ns/2.
:- multifile
	rdf_db:ns/2.

rdf_db:ns(vra,	   'http://www.swi.psy.uva.nl/mia/vra#').
rdf_db:ns(aat,	   'http://www.swi.psy.uva.nl/mia/aat#').
rdf_db:ns(ulan,	   'http://www.swi.psy.uva.nl/mia/ulan#').
rdf_db:ns(wn,	   'http://www.cogsci.princeton.edu/~wn/concept#').
rdf_db:ns(wns,	   'http://www.cogsci.princeton.edu/~wn/schema/').
rdf_db:ns(paint,   'http://www.swi.psy.uva.nl/mia/painting#').
rdf_db:ns(subject, 'http://www.swi.psy.uva.nl/mia/subject#').
rdf_db:ns(ic,	   'http://www.swi.psy.uva.nl/mia/iconclass#').
rdf_db:ns(ghs,	   'http://www.swi.psy.uva.nl/mia/ghs#').
rdf_db:ns(vin,     'http://www.w3.org/2001/sw/WebOnt/guide-src/wine#').
rdf_db:ns(food,    'http://www.w3.org/2001/sw/WebOnt/guide-src/food#').

:- load_files([ library(rdf),		% parser
		semweb(rdf_db),		% triple store
		semweb(rdfs),		% RDFS rules
		semweb(rdf_edit),	% transactions and changes
		rdf_text,		% basic text representation
		rules,			% rendering rules
		rdfs_explorer,		% visualization
		concur			% concurrency
%		rdf_portray
	      ],
	      [ silent(true)
	      ]).

:- pce_image_directory(icons).

:- pce_autoload(ulan_timestamp_object_item,
		library(ulan)).


rdf_file(base(rdfs),	 'rdfs.rdfs').
rdf_file(base(owl),	 'owl.owl').
rdf_file(base(dc),	 'dc.rdfs').
rdf_file(base(vra),	 'vra.owl').
rdf_file(base(painting), 'subject.owl').
rdf_file(base(painting), 'painting.owl').

rdf_file(aat,		 'aatmeta.rdfs').
rdf_file(aat,		 'aat.rdfs').

rdf_file(ulan,		 'ulan.rdfs').
rdf_file(ulan,		 'ulan.rdf').

rdf_file(wn,		 'wordnet-20000620.rdfs').
rdf_file(wn,		 'wordnet_glossary-20010201.rdf').
rdf_file(wn,		 'wordnet_hyponyms-20010201.rdf').
rdf_file(wn,		 'wordnet_nouns-20010201.rdf').
rdf_file(wn,		 'wordnet_similar-20010201.rdf').
rdf_file(wn,		 'wnclass.rdfs').

rdf_file(ic,		 'iconclass.rdfs').

load(Category) :-
	load(1, Category).
load(C, Category) :-				% load the whole world
	findall(rdf_load(X), rdf_file(Category, X), Goals),
	concurrent(C, Goals).

% :- rdf_debug(1).		% Print messages
:- load(base(rdfs)).
:- load(base(owl)).

world :-
	load(_).

ulan :-
	load(ulan).

aat :-
	load(aat).

wn :-
	load(wn).

dbg :-
%	debug(render),
	guitracer.

%:- nav.
%:- dbg.

:- catch(['~/.xpcerc'], _, true).

rdf_file_extension(rdf).
rdf_file_extension(rdfs).
rdf_file_extension(owl).


go :-
%	rdfe_open_journal(journal, append),
	new(X, rdfs_explorer),
	send(X, open).

go(['--world'|T]) :-
	world,
	go(T).
go(['--aat'|T]) :-
	aat,
	go(T).
go(['--wn'|T]) :-
	wn,
	go(T).
go(['--ulan'|T]) :-
	load(ulan),
	go(T).
go([File|T]) :-
	file_name_extension(_, Ext, File),
	rdf_file_extension(Ext), !,
	rdf_load(File),
	go(T).
go([]) :-
	new(X, rdfs_explorer),
	send(X, open).
go([File]) :-
	file_name_extension(_Base, rdfj, File),
	rdfe_open_journal(File, append),
	new(X, rdfs_explorer),
	send(X, open).


select :-
	use_module(rdfs_resource_item),
	new(D, dialog('Select')),
	send(D, append,
	     rdfs_resource_item(test)),
	send(D, open).


save(X) :-
	qsave_program(X, [class(development)]).
