/*  File:    rules.pl
    Author:  Jan Wielemaker
    Created: Jun 25 2003
    Purpose: Define rendering and other rules
*/

:- module(rdf_rules,
	  [
	  ]).
:- use_module(library(debug)).
:- use_module(particle).
:- use_module(semweb(rdfs)).
:- use_module(semweb(rdf_db)).
:- use_module(semweb(rdf_edit)).
:- use_module(rdf_text).
:- use_module(rdf_label).
:- use_module(rdf_cache).
:- use_module(rdf_util).


owl_description_attribute(X) :- rdf_equal(owl:oneOf, X).
owl_description_attribute(X) :- rdf_equal(owl:complementOf, X).
owl_description_attribute(X) :- rdf_equal(owl:unionOf, X).
owl_description_attribute(X) :- rdf_equal(owl:intersectionOf, X).

		 /*******************************
		 *	       LABELS		*
		 *******************************/

:- begin_particle(rdf_label_rules, []).

:- dynamic
	view_label_as/1.

label_text(Resource, Text) :-
	view_label_as(label_only), !,
	rdfs_label(Resource, Text).
label_text(Resource, Text) :-
	view_label_as(resource), !,
	rdf_global_id(NS:Local, Resource),
	concat_atom([NS, :, Local], Text).
label_text(Resource, Text) :-
	rdfs_ns_label(Resource, Text).

label(Resource, Label) :-
	::label_class(Resource, Class), !,
	Term =.. [Class, Resource],
	new(Label, Term).

%	label_class(+Resource, -Class) 
%	
%	Determine the visualiser to use for a short textual description
%	of a resource.  Resource is the resource for which to create a 
%	visualisation.  Role is one of subject, predicate or object and
%	Class is the XPCE class to use.

label_class(literal(_), rdf_literal_text) :- !.
label_class(Obj, ulan_timestamp_label) :-
	rdfs_individual_of(Obj, ulan:'TimeStamp').
label_class(Obj, wn_class_label) :-
	rdfs_individual_of(Obj, wns:'LexicalConcept').
label_class(Obj, rdf_property_label) :-
	rdfs_individual_of(Obj, rdf:'Property').
label_class(Obj, owl_restriction_label) :-
	rdfs_individual_of(Obj, owl:'Restriction').
label_class(Obj, LabelClass) :-
	rdfs_individual_of(Obj, owl:'Class'),
	(   owl_description_attribute(Att),
	    rdf_has(Obj, Att, _)
	->  LabelClass = owl_description_label
	;   LabelClass = owl_class_label
	).
label_class(Obj, LabelClass) :-
	rdfs_individual_of(Obj, rdfs:'Class'), !,
	(   rdfs_subclass_of(Obj, rdfs:'Class')
	->  LabelClass = rdfs_metaclass_label
	;   LabelClass = rdfs_class_label
	).
label_class(Obj, rdf_list_label) :-
	rdfs_individual_of(Obj, rdf:'List').
label_class(Obj, rdf_individual_label) :-
	rdf_has(Obj, rdf:type, _).
label_class('__not_filled', rdf_not_filled_label).
label_class(_, rdf_resource_text).

:- end_particle.

resource(class,       image, image('16x16/class.xpm')).
resource(metaclass,   image, image('16x16/Metaclass.gif')).
resource(orphanclass, image, image('16x16/orphanclass.xpm')).
resource(orphanres,   image, image('16x16/orphanres.xpm')).
resource(individual,  image, image('16x16/Instance.gif')).
resource(property,    image, image('16x16/SlotDirect.gif')).
resource(list,        image, image('16x16/list.xpm')).
resource(list_member, image, image('16x16/list_member.xpm')).
resource(untyped,     image, image('16x16/untyped.xpm')).
resource(resource,    image, image('16x16/resource.xpm')).
resource(restriction, image, image('16x16/restriction.xpm')).
resource(description, image, image('16x16/description.xpm')).
resource(wnclass,     image, image('16x16/wnclass.xpm')).
resource(nil,         image, image('16x16/DisketteBoxEmpty.xpm')).

:- begin_particle(rdf_icon_rules, []).

icon(R, Icon) :-
	rdfs_individual_of(R, wns:'LexicalConcept'),
	new(Icon, image(resource(wnclass))).
icon(R, Icon) :-
	rdfs_individual_of(R, rdfs:'Class'), !,
	(   rdfs_individual_of(R, owl:'Restriction')
	->  ResName = restriction
	;   rdfs_subclass_of(R, rdfs:'Class')
	->  ResName = metaclass
	;   owl_description_attribute(Att),
	    rdf_has(R, Att, _)
	->  ResName = description
	;   ResName = class
	),
	new(Icon, image(resource(ResName))).
icon(R, Icon) :-
	rdfs_individual_of(R, rdf:'Property'), !,
	new(Icon, image(resource(property))).
icon(R, Icon) :-
	rdfs_individual_of(R, rdf:'List'),
	new(Icon, image(resource(list))).
icon('__not_filled', Icon) :-
	new(Icon, image(resource(nil))).
icon(_, Icon) :-
	new(Icon, image(resource(individual))).

:- end_particle.


:- begin_particle(rdf_resource_menu, []).

popup(Gr, Popup) :-
	new(Popup, popup),
	(   bagof(Item, ::menu_item(Gr, _Group, Item, Receiver), Items),
	    send(Popup, append, gap),
	    forall(item_member(Method, Label, Items),
		   append_item(Popup, Label, Receiver, Method)),
	    fail
	;   true
	).

append_item(Popup, Label, Receiver, Method) :-
	Method =.. List,
	Message =.. [message, Receiver | List],
	send(Popup, append, menu_item(Label, Message)).

container_with_method(Gr, Message, Gr) :-
	functor(Message, Method, _),
	send(Gr, has_send_method, Method).
container_with_method(Gr, Method, Container) :-
	get(Gr, contained_in, Container0),
	container_with_method(Container0, Method, Container).

item_method(Item=Method, Label, Method) :- !,
	Item =.. List,
	concat_atom(List, '_', Label).
item_method(Item,        Label, Item) :-
	Item =.. List,
	concat_atom(List, '_', Label).

item_member(Method, Label, Items) :-
	member(Item, Items),
	item_method(Item, Label, Method).

menu_item(Gr, Group, Item, Receiver) :-
	::menu_item(Group, Item),
	item_method(Item, _Label, Method),
	(   container_with_method(Gr, Method, Receiver)
	->  debug(menu, '~p: mapping ~w to ~p->~w',
		  [Gr, _Label, Receiver, Method])
	;   debug(menu, '~p: no container implements ->~w',
		  [Gr, Method]),
	    fail
	).

%	menu_item(Group, Item)
%	
%	Define menu items.  When  creating  the   popup  all  items  are
%	collected by Group which are separated by a bar. The ordering is
%	left unchanged.
%	
%	Item is either a method name or of the form LabelName=Method

menu_item(select, hierarchy_location).
menu_item(select, details).

menu_item(copy,   show_id).
menu_item(copy,   copy_id).
menu_item(copy,   copy_as_xml_identifier).
menu_item(copy,   copy_as_xml_attribute).
menu_item(copy,   copy_text).

menu_item(open,   view_rdf_source).
menu_item(open,   diagram_).

:- end_particle.


		 /*******************************
		 *		EDIT		*
		 *******************************/

:- begin_particle(rdf_predicate, []).

%	standard_predicate(+Resource, -Predicate)
%	
%	Return, on backtracking, predicates that should normally be defined
%	immediately when defining a resource.  This should use properties of
%	the ontology, but often it doesn't.

standard_predicate(Resource, Pred) :-
	rdfs_individual_of(Resource, rdf:'Statement'), !,
	(   rdf_equal(Pred, rdf:'Subject')
	;   rdf_equal(Pred, rdf:'Predicate')
	;   rdf_equal(Pred, rdf:'Object')
	).

:- end_particle.





		 /*******************************
		 *	      HIERARCHY		*
		 *******************************/

:- begin_particle(class_hierarchy, []).

%	child_cache(+Resource, -Cache, -Class)
%	
%	Create a cache (see rdf_cache/3) for generating the childs of
%	Resource.  The child-nodes are created as instances of Class.

child_cache(R, Cache, Class) :-
	rdfs_individual_of(R, rdfs:'Class'),
	(   rdfs_subclass_of(R, rdf:'Property')
	->  (   rdf_cache(lsorted(V), rdf_has(V, rdfs:subClassOf, R), Cache),
	        Class = rdf_class_node
	    ;   rdf_cache(lsorted(V), root_property(R,V), Cache),
		Class = rdf_property_node
	    )
	;   (   rdf_cache(lsorted(V), rdf_has(V, rdfs:subClassOf, R), Cache),
	        Class = rdf_class_node
	    ;   \+ rdfs_subclass_of(R, rdfs:'Class'),
	        rdf_cache(lsorted(V), rdf_has(V, rdf:type, R), Cache),
		Class = rdf_individual_node
	    ;	rdfs_subclass_of(R, owl:'Restriction'),
		rdf_cache(lsorted(V), rdf_has(V, rdf:type, R), Cache),
		Class = owl_restriction_node	    
	    )
	).
child_cache(R, Cache, rdf_individual_node) :-
	rdfs_individual_of(R, rdf:'List'), !,
	rdf_cache(lsorted(V), rdfs_member(V, R), Cache).


%	parent(+Resource, -Parent, -Class)
%	
%	Find parent relations to expand the hierarchy selectively for
%	showing Resource.

parent(R, Parent, rdf_class_node) :-
	rdf_has(R, rdfs:subClassOf, Parent), !.
parent(R, Parent, rdf_property_node) :-
	rdf_has(R, rdfs:subPropertyOf, Parent), !.
parent(R, Parent, rdf_individual_node) :-
	rdf_has(R, rdf:type, Parent), !.
parent('__orphan_classes', Root, rdf_orphan_node) :- !,
	rdf_equal(Root, rdfs:'Resource').
parent('__orphan_resources', Root, rdf_orphan_node) :- !,
	rdf_equal(Root, rdfs:'Resource').
parent(R, '__orphan_classes', rdf_class_node) :-
	rdfs_individual_of(R, rdfs:'Class'), !.
parent(_, '__orphan_resources', rdf_individual_node).

%	root_property(+Class, -Property)
%	
%	Generate the instances of Class (a subclass of rdf:Property)
%	that have no super property.

root_property(Class, P) :-
	rdf_has(P, rdf:type, Class),
	\+ rdf_has(P, rdfs:subPropertyOf, _).

:- end_particle.


		 /*******************************
		 *	   DRAG-AND-DROP	*
		 *******************************/

:- begin_particle(rdf_drag_and_drop, []).

%	drop(Graphical, Resource)
%	
%	Drop a resource on a graphical.  Determines the possible commands
%	and executes ::drop(Command, Graphical, Resource)

drop(Gr, V) :-
	(   send(@event, instance_of, event),
	    send(@event, is_a, ms_right_up)
	->  findall(Cmd, ::drop_command(Gr, V, Cmd), List),
	    get(@receiver, select_command, List, Cmd)
	;   ::drop_command(Gr, V, Cmd)
	->  true
	),
	rdfe_transaction(::drop(Cmd, Gr, V), Cmd).

drop(Command, Gr, V) :-
	send(Gr, has_get_method, resource),
	send(V, has_get_method, resource),
	get(Gr, resource, C),
	get(V, resource, R),
	::drop_resource(Command, C, R).

drop_resource(move_class, C, R) :- !,			% drop R on C
	rdfe_retractall(R, rdfs:subClassOf, _),
	rdfe_assert(R, rdfs:subClassOf, C).
drop_resource(add_class, C, R) :- !,
	rdfe_assert(R, rdfs:subClassOf, C).
drop_resource(move_property, C, R) :- !,
	rdfe_retractall(R, rdfs:subPropertyOf, _),
	rdfe_assert(R, rdfs:subPropertyOf, C).
drop_resource(change_type, C, R) :- !,
	rdfe_retractall(R, rdf:type, _),
	rdfe_assert(R, rdf:type, C).
drop_resource(add_type, C, R) :- !,
	rdfe_assert(R, rdf:type, C).
drop_resource(Command, Graphical, Resource) :-
	format('TBD: Drop ~w onto ~p: ~w~n',
	       [Resource, Graphical, Command]).

drop_command(Gr, V, Command) :-
	send(Gr, has_get_method, resource),
	send(V, has_get_method, resource),
	get(Gr, resource, C),
	get(V, resource, R),
	::drop_resource_command(C, R, Command).

drop_resource_command(C, R, move_property) :-
	rdfs_individual_of(C, rdf:'Property'),
	rdfs_individual_of(R, rdf:'Property'), !.
drop_resource_command(C, R, move_class) :-
	rdfs_individual_of(C, rdfs:'Class'),
	rdfs_individual_of(R, rdfs:'Class').
drop_resource_command(C, R, add_class) :-
	rdfs_individual_of(C, rdfs:'Class'),
	rdfs_individual_of(R, rdfs:'Class').
drop_resource_command(C, R, change_type) :-
	rdfs_individual_of(C, rdfs:'Class'),
	\+ rdfs_individual_of(R, rdfs:'Class').

:- end_particle.

:- begin_particle(rdf_click, []).

clicked(V) :-
	get(V, resource, R),
	format('Clicked ~p representing ~p~n', [V, R]).

:- end_particle.



		 /*******************************
		 *	  BIND TO OBJECTS	*
		 *******************************/

:- begin_particle(display,
		  [ rdf_label_rules,
		    rdf_icon_rules,
		    class_hierarchy,
		    rdf_resource_menu,
		    rdf_drag_and_drop,
		    rdf_click,
		    rdf_predicate
		  ]).
:- end_particle.


		 /*******************************
		 *	     HIERARCHY		*
		 *******************************/

:- begin_particle(rdf_tree, display).

clicked(V) :-
	get(V, resource, R),
	get(V, container, rdf_node, Node),
	(   get(Node, resource, R)
	->  send(Node?tree, selected, Node)
	;   super::clicked(V)
	).

menu_item(Group, Item) :-
	super::menu_item(Group, Item).
menu_item(edit, unrelate=unrelate_resource).
menu_item(edit, delete=delete_resource).

menu_item(Gr, edit, new(Role), Node) :-
	(   container_with_method(Gr, new, Node),
	    send(Node, instance_of, rdf_node)
	->  get(Node?caches, attribute_names, Roles),
	    chain_list(Roles, List),
	    member(Role, List)
	).
menu_item(Gr, Group, Item, Receiver) :-
	super::menu_item(Gr, Group, Item, Receiver),
	Item \== hierarchy_location.

:- end_particle.

:- begin_particle(rdf_node, rdf_tree).

%	Drop onto a node in the hierarchy

drop(move_class, Onto, From) :-
	get(From, triple, rdf(S, P, O)),
	get(From, resource, S),
	get(Onto, resource, New),
	rdf_has(S, rdfs:subClassOf, O, P), !,
	rdfe_transaction(rdfe_update(S, P, O, object(New)),
			 move_class).
drop(change_type, Onto, From) :-
	get(From, triple, rdf(S, P, O)),
	get(From, resource, S),
	get(Onto, resource, New),
	rdf_has(S, rdf:type, O, P), !,
	rdfe_transaction(rdfe_update(S, P, O, object(New)),
			 change_type).
drop(Command, Onto, From) :-
	super::drop(Command, Onto, From).

:- end_particle.


:- begin_particle(rdf_individual_node, rdf_node).

:- end_particle.

:- begin_particle(rdf_property_node, rdf_node).

child_cache(R, Cache, rdf_property_node) :-
	rdf_cache(lsorted(V), rdf_has(V, rdfs:subPropertyOf, R), Cache).

:- end_particle.

:- begin_particle(rdf_root_node, rdf_node).

child_cache(R, Cache, Role) :-
	rdf_equal(R, rdfs:'Resource'),
	(   rdf_cache(lsorted(V), rdf_has(V, rdfs:subClassOf, R), Cache),
	    Role = rdf_class_node
	;   \+ rdfs_subclass_of(R, rdfs:'Class'),
	    rdf_cache(lsorted(V), rdf_has(V, rdf:type, R), Cache),
	    Role = rdf_individual_node
	).
child_cache(_, Cache, rdf_orphan_node) :-
	rdf_cache(X, orphan_resource(X), Cache).

orphan_resource('__orphan_classes') :-
	rdf_orphan_node:orphan_class(_) -> true.
orphan_resource('__orphan_resources') :-
	rdf_orphan_node:orphan_resource(_) -> true.

:- end_particle.


:- begin_particle(rdf_orphan_node, rdf_node).

label_text('__orphan_classes', '<Classes without rdfs:subClassOf>').
label_text('__orphan_resources', '<Resources without rdf:type>').
%label_text(Resource, Label) :-
%	super::label_text(Resource, Label).

icon('__orphan_classes', Icon) :-
	new(Icon, image(resource(orphanclass))).
icon('__orphan_resources', Icon) :-
	new(Icon, image(resource(orphanres))).
icon(Resource, Icon) :-
	super::icon(Resource, Icon).

label_class('__orphan_classes', rdfs_class_label).
label_class('__orphan_resources', rdf_individual_label).
label_class(Resource, Class) :-
	super::label_class(Resource, Class).

child_cache('__orphan_classes', Cache, rdf_class_node) :-
	rdf_cache(lsorted(X), orphan_class(X), Cache).
child_cache('__orphan_resources', Cache, rdf_individual_node) :-
	rdf_cache(lsorted(X), orphan_resource(X), Cache).
child_cache(Resource, Cache, Role) :-
	super::child_cache(Resource, Cache, Role).

orphan_class(Class) :-
	rdfs_individual_of(Class, rdfs:'Class'),
	\+ rdf_has(Class, rdfs:subClassOf, _),
	\+ rdf_equal(Class, rdfs:'Resource').

orphan_resource(Resource) :-
	rdf_subject(Resource),
	\+ rdf_has(Resource, rdf:type, _).
orphan_resource(Resource) :-
	rdf(_, _, Resource),
	atom(Resource),
	\+ rdf_has(Resource, rdf:type, _).

:- end_particle.


		 /*******************************
		 *	      TABLE		*
		 *******************************/

:- begin_particle(rdf_tabular, display).

clicked(V) :-
	send(V?device, selection, V).

:- end_particle.

:- begin_particle(rdf_object_cell, rdf_tabular).

menu_item(Group, Item) :-
	super::menu_item(Group, Item).
menu_item(edit, modify).
menu_item(edit, delete).

drop_command(_Me, _Resource, modify) :-
	true.				% must validate restrictions!

drop(modify, Gr, V) :-
	get(V, resource, Resource),
	get(Gr, triple, rdf(Subject, Predicate, Old)),
	rdf_set_object(Subject, Predicate, Old, Resource).

:- end_particle.


:- begin_particle(rdf_predicate_cell, rdf_tabular).

drop_command(_Me, _Resource, add) :-
	true.				% must validate restrictions!

drop(add, Gr, V) :-
	get(V, resource, Resource),	
	get(Gr, triple, rdf(Subject, Predicate, _)),
	rdfe_transaction(rdfe_assert(Subject, Predicate, Resource),
			 add_property).

:- end_particle.

:- begin_particle(rdf_range_cell, rdf_object_cell).

drop(modify, _Gr, V) :-
	get(V, resource, Resource),
	get(@particle, triple, rdf(Subject, Predicate, Old)),
	rdf_set_object(Subject, Predicate, Old, Resource).

:- end_particle.


:- begin_particle(rdf_domain_cell, rdf_object_cell).

drop(modify, _Gr, V) :-
	get(V, resource, Resource),
	get(@particle, triple, rdf(Subject, Predicate, Old)),
	rdf_set_object(Subject, Predicate, Old, Resource).

:- end_particle.


:- begin_particle(rdf_not_filled_label, rdf_resource_menu).

menu_item(edit, delete).
menu_item(edit, modify).
menu_item(edit, make_literal=type(literal)).

clicked(V) :-
	send(V, modify).

:- end_particle.
