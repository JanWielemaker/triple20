/*  File:    rdf_render.pl
    Author:  Jan Wielemaker
    Created: Feb 26 2003
    Purpose: Rendering support primitives
*/

:- module(rdf_render,
	  [ rdf_render_object_with/3,	% +Value, +Container, -PceClass
	    rdf_render_value_with/3,	% +Value, +Container, -PceClass
	    rdf_render_child_of/3,	% +Resource, +Context, -Child
	    rdf_render_child_relation/3,% +Resource, +Context, -Child
	    rdf_render_parent_relation/3, % +Resource, +Context, -Parent
	    rdf_render_node_with/3	% +Value, +Container, -PceClass
	  ]).
:- use_module(library(pce)).
:- use_module(library(pce_meta)).
:- use_module(semweb(rdfs)).
:- use_module(owl).
:- use_module(library(debug)).

		 /*******************************
		 *	OBJECT RENDERING	*
		 *******************************/

%	rdf_render_object_with(rdf(S,P,O), +Container, -RenderClass)
%	
%	Determine which XPCE class to use for rendering the object
%	of a triple.  Currently only uses the object, but could use
%	information from the subject and predicate.

rdf_render_object_with(rdf(_S,_P,Value), Container, Class) :- !,
	rdf_render_value_with(Value, Container, Class).

%	rdf_render_value_with(+Value, +Container, -RenderClass)
%	
%	Determine which XPCE class to use for rendering Value in the
%	context of Container.

rdf_render_value_with(Value, Container, Class) :-
	findall(Domain-Class,
		can_render(Value, Container, Class, Domain),
		Pairs),
	best(Pairs, -, _Domain-Class),
	debug(render, 'Redendering ~p inside ~p with ~w',
	      [ Value, Container, Class ]).
rdf_render_value_with(Value, Container, _Class) :-
	debug(render, 'No rendering defined for ~p inside a ~p',
	      [ Value, Container ]),
	fail.

%	best([RDFClass-PceClass, ...], RDFClass-PceClass)
%	
%	Determine the best match.  This is defined to be the match with
%	the most specific RDFClass and if these two match the most specific
%	XPCE container class.

best([], Best, Best).
best([H|T], Best0, Best) :-
	best(H, Best0, Best1),
	best(T, Best1, Best).

best(Best1, Best2, Best) :-
	Best1 = C1-CC1,
	Best2 = C2-CC2, !,
	(   (   C1 == C2
	    ->  isa_class(CC1, CC2)
	    ;	(   rdf_equal(Best2, rdfs:'Resource')
		;   rdfs_subclass_of(C1, C2)
		)
	    )
	->  Best = Best1
	;   Best = Best2
	).
best(Best, -, Best).


%	can_render(+Value, +Container, -Class, -Domain)
%	
%	True if Class (XPCE) can used to render Value inside Container.
%	Domain returns the domain of the render specification that is
%	used.

can_render(Value, Container, Class, Domain) :-
	rdfs_individual_of(Render, tool:'Render'),
	rdf_has(Render, tool:role, literal(object)),
	rdf_has(Render, tool:domain, Domain),
	in_domain(Value, Domain),
	rdf_has(Render, tool:context, literal(ContainerClass)),
	contained_in(Container, InSide),
	get(InSide, class_name, GroundClass),
	isa_class(GroundClass, ContainerClass),
	rdf_has(Render, tool:class, literal(Class)).

contained_in(Container, Container).
contained_in(Container, Outer) :-
	get(Container, contained_in, Parent),
	contained_in(Parent, Outer).

in_domain(literal(_), Domain) :- !,
	rdfs_subclass_of(rdfs:'Literal', Domain).
in_domain(Resource, Domain) :-
	atom(Resource),
	(   rdf_equal(Domain, rdfs:'Resource')
	->  true
	;   owl_individual_of(Resource, Domain)
	).


		 /*******************************
		 *	    HIERARCHIES		*
		 *******************************/

%	rdf_render_child_of(+Resource, +Context, -Child)
%	
%	Enumerate the children of Resource in the given context.  This
%	generates all nodes linked to Resource using one of the
%	child-relations applicable.

rdf_render_child_of(Resource, Context, Child) :-
	rdf_render_child_relation(Resource, Context, ChildRel),
	rdf_has(Child, ChildRel, Resource).
	
rdf_render_child_relation(Resource, Context, ChildRel) :-
	rdfs_individual_of(Render, tool:'Render'),
	rdf_has(Render, tool:context, literal(Context)),
	rdf_has(Render, tool:domain, Domain),
	in_domain(Resource, Domain), !,
	rdf_has(Render, tool:'child-relation', ChildRels),
	rdfs_member(ChildRel, ChildRels).

rdf_render_parent_relation(Resource, Context, ParentRel) :-
	rdfs_individual_of(Render, tool:'Render'),
	rdf_has(Render, tool:context, literal(Context)),
	rdf_has(Render, tool:domain, Domain),
	in_domain(Resource, Domain), !,
	rdf_has(Render, tool:'parent-relation', ParentRels),
	rdfs_member(ParentRel, ParentRels).

rdf_render_node_with(Resource, Context, PceClass) :-
	rdf_render_value_with(Resource, Context, PceClass).

