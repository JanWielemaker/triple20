/*  File:    anon.pl
    Author:  Jan Wielemaker
    Created: Nov 20 2003
    Purpose: Handle anonymous instances
*/

:- module(rdf_anon, []).
:- use_module(library(pce)).
:- use_module(semweb(rdf_db)).
:- use_module(triple20(rdf_rules)).

:- pce_begin_class(rdf_anon_label, rdf_individual_label,
		   "Display anonymous resource").
:- pce_end_class(rdf_anon_label).

:- begin_rules(display, anon).

label_class(Resource, rdf_anon_label) :-
	anon_individual(Resource, _), !.
label_class(Resource, Class) :-
	super::label_class(Resource, Class).

label_text(Resource, Text) :-
	anon_individual(Resource, Class), !,
	super::label_text(Class, Text).
label_text(Resource, Text) :-
	super::label_text(Resource, Text).

:- end_rules.

:- begin_rules(rdf_anon_label, anon).

resource(anon, image, image('16x16/anon.xpm')).

icon(Resource, Icon) :-
	anon_individual(Resource, _Class), !,
	new(Icon, image(resource(anon))).
icon(Resource, Icon) :-
	super::icon(Resource, Icon).

:- end_rules.

anon_individual(Resource, Class) :-
	rdf_is_bnode(Resource),
	rdf_equal(rdf:type, TypeP),
	rdf(Resource, TypeP, Class),
	\+ (  rdf(Resource, P, O),
	      (	  P \== TypeP
	      ;	  O \== Class
	      )
	   ).

