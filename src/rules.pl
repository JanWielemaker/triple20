/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(rdf_default_rules,
	  [
	  ]).
:- use_module(library(debug)).
:- use_module(particle).
:- use_module(rdf_rules).		% Get call_outer.  Must move
:- use_module(semweb(rdfs)).
:- use_module(semweb(rdf_db)).
:- use_module(semweb(rdf_edit)).
:- use_module(library(broadcast)).
:- use_module(rdf_text).
:- use_module(rdf_label).
:- use_module(rdf_cache).
:- use_module(rdf_util).
:- use_module(owl).


owl_description_attribute(X) :- rdf_equal(owl:oneOf, X).
owl_description_attribute(X) :- rdf_equal(owl:complementOf, X).
owl_description_attribute(X) :- rdf_equal(owl:unionOf, X).
owl_description_attribute(X) :- rdf_equal(owl:intersectionOf, X).

		 /*******************************
		 *	       LABELS		*
		 *******************************/

:- begin_rules(rdf_label_rules, default).

:- dynamic
	view_label_as_setting/1.

view_label_as_setting(namespace_and_label).	% initial default

label_text(Resource, Text) :-
	view_label_as_setting(label_only), !,
	rdfs_label(Resource, Text).
label_text(Resource, Text) :-
	view_label_as_setting(resource), !,
	rdf_global_id(NS:Local, Resource),
	concat_atom([NS, :, Local], Text).
label_text(Resource, Text) :-
	rdfs_ns_label(Resource, Text).

%	view_label_as(?Style)
%	
%	This is not a rule, but intended to  be called to change the way
%	labels are presented to the user.

view_label_as(As) :-
	view_label_as_setting(As), !.
view_label_as(As) :-
	retractall(view_label_as_setting(_)),
	assert(view_label_as_setting(As)),
	send(@resource_texts, for_all,
	     message(@arg2, for_all,
		     message(@arg1, update))),
	broadcast(view_label_as(As)).


label(Resource, Label) :-
	inner::label_class(Resource, Class), !,
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
label_class(_, rdf_resource_label).

:- end_rules.

:- multifile
	resource/3.

resource(class,       image, image('16x16/class.xpm')).
resource(litclass,    image, image('16x16/doc.xpm')).
resource(metaclass,   image, image('16x16/Metaclass.gif')).
resource(orphanclass, image, image('16x16/orphanclass.xpm')).
resource(orphanres,   image, image('16x16/orphanres.xpm')).
resource(individual,  image, image('16x16/instance.xpm')).
resource(resource,    image, image('16x16/resource.xpm')).
resource(property,    image, image('16x16/SlotDirect.gif')).
resource(list,        image, image('16x16/list.xpm')).
resource(list_item,   image, image('16x16/list_member.xpm')).
resource(untyped,     image, image('16x16/untyped.xpm')).
resource(resource,    image, image('16x16/resource.xpm')).
resource(restriction, image, image('16x16/restriction.xpm')).
resource(description, image, image('16x16/description.xpm')).
resource(wnclass,     image, image('16x16/wnclass.xpm')).
resource(nil,         image, image('16x16/DisketteBoxEmpty.xpm')).
resource(part,        image, image('16x16/part.xpm')).
resource(inferred,    image, image('16x16/think.xpm')).

:- begin_rules(rdf_icon_rules, default).

icon(R, Icon) :-
	inner::icon_resource(R, Resource),
	new(Icon, image(resource(Resource))).

icon_resource(R, wnclass) :-
	rdfs_individual_of(R, wns:'LexicalConcept'), !.
icon_resource(R, ResName) :-
	rdfs_individual_of(R, rdfs:'Class'), !,
	(   rdfs_individual_of(R, owl:'Restriction')
	->  ResName = restriction
	;   rdfs_subclass_of(R, rdfs:'Class')
	->  ResName = metaclass
	;   owl_description_attribute(Att),
	    rdf_has(R, Att, _)
	->  ResName = description
	;   rdfs_subclass_of(R, rdfs:'Literal')
	->  ResName = litclass
	;   ResName = class
	).
icon_resource(R, property) :-
	rdfs_individual_of(R, rdf:'Property'), !.
icon_resource(R, list) :-
	rdfs_individual_of(R, rdf:'List'), !.
icon_resource('__not_filled', nil) :- !.
icon_resource(R, individual) :-
	rdf_has(R, rdf:type, _), !.
icon_resource(_, resource).


:- end_rules.


:- begin_rules(rdf_resource_menu, default).

popup(Gr, Popup) :-
	new(Popup, popup),
	(   bagof(Item, inner::menu_item(Gr, Group, Item, Receiver), Items),
	    (	inner::sub_menu(Group)
	    ->	(   get(Popup?members?tail, popup, @nil)
		->  send(Popup, append, gap)
		;   true
		),
	        send(Popup, append, new(SubMenu, popup(Group))),
		forall(item_member(Method, Label, Items),
		       append_item(SubMenu, Label, Receiver, Method))
	    ;	send(Popup, append, gap),
		forall(item_member(Method, Label, Items),
		       append_item(Popup, Label, Receiver, Method))
	    ),
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
	(   item_label(Item, Label)
	->  true
	;   Item =.. List,
	    concat_atom(List, '_', Label)
	).
item_method(Item,        Label, Item) :-
	(   item_label(Item, Label)
	->  true
	;   Item =.. List,
	    concat_atom(List, '_', Label)
	).

%	item_label(+Item, -Label)
%	
%	Items that need a dedicated label.

item_label(new(rdf_class_node), new_subclass).
item_label(new(rdf_individual_node), new_individual).
item_label(new(rdf_property_node), new_property).
item_label(new(owl_restriction_node), new_restriction).

item_member(Method, Label, Items) :-
	member(Item, Items),
	item_method(Item, Label, Method).

menu_item(Gr, Group, Item, Receiver) :-
	inner::menu_item(Group, Item),
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

menu_item(view,   view_rdf_source).
menu_item(view,   diagram_).
menu_item(view,   triples=view_triples).

menu_item(edit,   rename_resource).
menu_item(edit,	  set_language).
menu_item(edit,   set_type).

%	sub_menu(Group)
%	
%	Group of items to place in a sub-menu

sub_menu(copy).
sub_menu(view).

:- end_rules.


		 /*******************************
		 *		EDIT		*
		 *******************************/

:- begin_rules(rdf_predicate, default).

%	standard_predicate(+Resource, -Predicate, -Default)
%	
%	Return, on backtracking, predicates  that   should  normally  be
%	defined immediately when defining a   resource.  This should use
%	properties of the  ontology,  but   often  it  doesn't.  Default
%	specifies the default value. When left unspecified (unbound) the
%	default is computed from the range of the property.

standard_predicate(Resource, Pred, _) :-
	rdfs_individual_of(Resource, rdf:'Statement'), !,
	(   rdf_equal(Pred, rdf:subject)
	;   rdf_equal(Pred, rdf:predicate)
	;   rdf_equal(Pred, rdf:object)
	).
standard_predicate(Resource, Pred, _) :-
	rdfs_individual_of(Resource, rdf:'Property'), !,
	(   rdf_equal(Pred, rdfs:range)
	;   rdf_equal(Pred, rdfs:domain)
	).
standard_predicate(Resource, Pred, _) :-
	rdfs_individual_of(Resource, owl:'Restriction'), !,
	rdf_equal(Pred, owl:onProperty).

%	visible_predicate(+Resource, -Predicate)
%	
%	Return, on backtracking, predicates that should be displayed
%	when showing a resource in a property table.

visible_predicate(Resource, Predicate) :-
	findall(P, rdf(Resource, P, _), Ps),
	sort_by_label(Ps, Predicates),
	member(Predicate, Predicates).

%	class_predicate(+Class, -Predicates)
%	
%	Return, on backtracking, proterties that are applicable to Class
%	and must be displayed on the sheet for this class.

class_predicate(Class, Predicate) :-
	findall(P, rdfs_class_property(Class, P), Ps),
	sort_by_label(Ps, Predicates),
	member(Predicate, Predicates).

%	rdf_default(+Subject, +Predicate, -Object)
%	
%	Provide a default value for a new Predicate on Subject

rdf_default(_, P, literal('1')) :-
	(   rdf_equal(P, owl:cardinality)
	;   rdf_equal(P, owl:maxCardinality)
	;   rdf_equal(P, owl:minCardinality)
	), !.
rdf_default(S, P, O) :-
	property_type(S, P, Type),
	default_object(Type, O).

default_object(list, Nil) :-
	rdf_equal(Nil, rdf:nil).
default_object(literal(_), literal('')).

:- end_rules.


		 /*******************************
		 *	      HIERARCHY		*
		 *******************************/

:- begin_rules(class_hierarchy, default).

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
	;   (   (   rdf_current_dialect(owl)
		->  rdf_cache(lsorted(V), owl_direct_subclass_of(V, R), Cache)
		;   rdf_cache(lsorted(V), rdf_has(V, rdfs:subClassOf, R), Cache)
		),
	        Class = rdf_class_node
	    ;   \+ rdfs_subclass_of(R, rdfs:'Class'),
		Class = rdf_individual_node,
		rdf_cache(lsorted(V), rdf_has(V, rdf:type, R), Cache)
	    ;	rdfs_subclass_of(R, owl:'Restriction'),
		rdf_cache(V, ordered_restriction(V, R), Cache),
		Class = owl_restriction_node
	    )
	).
child_cache(R, Cache, rdf_inferred_node) :-
	inner::view_owl_class_extension,
	rdfs_individual_of(R, owl:'Class'),
	\+ rdfs_subclass_of(R, rdfs:'Class'),
	rdf_cache(lsorted(V), owl_inferred_member(V, R), Cache).
child_cache(R, Cache, rdf_list_item_node) :-
	rdfs_individual_of(R, rdf:'List'), !,
	rdf_cache(lsorted(V), rdfs_member(V, R), Cache).
child_cache(R, Cache, rdf_part_node) :-	% TBD: move outside
	rdf_has(erc:has_part, rdfs:domain, Domain),
	rdf_has(R, rdf:type, Class),
	rdfs_subclass_of(Class, Domain),
	rdf_cache(lsorted(V), rdf_has(R, erc:has_part, V), Cache).
child_cache(R, Cache, rdf_part_node) :-	% TBD: move outside
	rdf_has(thales:hasPart, rdfs:domain, Domain),
	rdf_has(R, rdf:type, Class),
	rdfs_subclass_of(Class, Domain),
	rdf_cache(lsorted(V), rdf_has(R, thales:hasPart, V), Cache).
child_cache(R, Cache, rdf_class_node) :-
	rdfs_individual_of(R, skos:'Concept'),
	rdf_cache(lsorted(V), skos_narrower(R, V), Cache).

skos_narrower(Class, Narrow) :-
	rdf_has(Narrow, skos:broader, Class).
skos_narrower(Class, Narrow) :-
	rdf_has(Narrow, rdfs:subClassOf, Class).
skos_narrower(Class, Narrow) :-
	rdf_has(Class, skos:narrower, Narrow).

%	setting predicate that can be overruled

view_owl_class_extension.

owl_inferred_member(R, Class) :-
	owl_individual_of(R, Class),
	atom(R),			% avoid literals
	\+ rdf_has(R, rdf:type, Class).

ordered_restriction(R, Class) :-
	findall(L-R, owl_restriction_with_label(Class, R, L), Pairs),
	keysort(Pairs, Sorted),
	rdf_util:unique_unkey(Sorted, Restrictions),
	member(R, Restrictions).

owl_restriction_with_label(Class, Restriction, Label) :-
	rdf_has(Restriction, rdf:type, Class),
	(   rdf_has(Restriction, owl:onProperty, Property)
	->  rdfs_label(Property, Label)
	;   rdfs_label(Restriction, Label)
	).


%	parent(+Resource, -Parent, -Class)
%	
%	Find parent relations to expand the hierarchy selectively for
%	showing Resource.

parent(R, Parent, rdf_class_node) :-
	(   rdf_current_dialect(owl)
	->  owl_direct_subclass_of(R, Parent)
	;   rdf_has(R, rdfs:subClassOf, Parent)
	).
parent(R, Parent, rdf_class_node) :-
	rdfs_individual_of(R, skos:'Concept'),
	skos_narrower(Parent, R).
parent(R, Parent, Role) :-
	rdf_has(R, rdf:type, Type),
	\+ rdfs_individual_of(R, rdfs:'Class'),
	(   rdfs_subclass_of(Type, rdf:'Property')
	->  Role = rdf_property_node,
	    (	rdf_has(R, rdfs:subPropertyOf, Parent)
	    ->	true
	    ;	Parent = Type
	    )
	;   Role = rdf_individual_node,
	    Parent = Type
	).
parent(R, Parent, rdf_part_node) :-
	rdf_has(Parent, erc:has_part, R).
parent('__orphan_classes', Root, rdf_orphan_node) :-
	rdf_equal(Root, rdfs:'Resource').
parent('__orphan_resources', Root, rdf_orphan_node) :-
	rdf_equal(Root, rdfs:'Resource').
parent(R, '__orphan_classes', rdf_class_node) :-
	rdfs_individual_of(R, rdfs:'Class'),
	\+ rdf_has(R, rdfs:subClassOf, _).
parent(R, '__orphan_resources', rdf_node) :-
	\+ rdf_has(R, rdf:type, _).

%	root_property(+Class, -Property)
%	
%	Generate the instances of Class (a subclass of rdf:Property)
%	that have no super property and are of the same type

root_property(Class, P) :-
	rdf_has(P, rdf:type, Class),
	\+ (  rdf_has(P, rdfs:subPropertyOf, P2),
	      rdf_has(P2, rdf:type, Class)
	   ).

:- end_rules.


		 /*******************************
		 *	   DRAG-AND-DROP	*
		 *******************************/

:- begin_rules(rdf_drag_and_drop, default).

%	drop(Graphical, Resource)
%	
%	Drop a resource on a graphical.  Determines the possible commands
%	and executes ::drop(Command, Graphical, Resource)

drop(Gr, V) :-
	(   send(@event, instance_of, event),
	    send(@event, is_a, ms_right_up)
	->  findall(Cmd, inner::drop_command(Gr, V, Cmd), List),
	    List \== [],
	    get(@receiver, select_command, List, Cmd)
	;   inner::drop_command(Gr, V, Cmd)
	->  true
	),
	catch(rdfe_transaction(inner::drop(Cmd, Gr, V), Cmd),
	      E, report_exception(Gr, E)).

report_exception(Gr, E) :-
	message_to_string(E, Message),
	send(Gr, report, error, Message).

drop(Command, Gr, V) :-
	send(Gr, has_get_method, resource),
	send(V, has_get_method, resource),
	get(Gr, resource, C),
	get(V, resource, R),
	inner::drop_resource(Command, C, R).

drop_resource(move_class, C, R) :- !,			% drop R on C
	rdf_set_object(R, rdfs:subClassOf, C).
drop_resource(add_class, C, R) :- !,
	rdf_add_object(R, rdfs:subClassOf, C).
drop_resource(move_property, C, R) :- !,
	rdf_set_object(R, rdfs:subPropertyOf, C).
drop_resource(change_type, C, R) :- !,
	rdf_set_object(R, rdf:type, C).
drop_resource(add_type, C, R) :- !,
	rdf_add_object(R, rdf:type, C).
drop_resource(make_skos_narrower, C, R) :- !,
	rdf_set_rev_object(R, skos:broader, skos:narrower, C).
drop_resource(Command, Graphical, Resource) :-
	format('TBD: Drop ~w onto ~p: ~w~n',
	       [Resource, Graphical, Command]).

drop_command(Gr, V, Command) :-
	send(Gr, has_get_method, resource),
	send(V, has_get_method, resource),
	get(Gr, resource, C),
	get(V, resource, R),
	C \== R,			% do not allow dropping on `self'
	inner::drop_resource_command(C, R, Command).

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
drop_resource_command(C, R, make_skos_narrower) :-
	rdfs_individual_of(C, skos:'Concept'),
	rdfs_individual_of(R, skos:'Concept').

%	drop_files(+V, +ListOfFiles)
%	
%	Deal with dropping files from the explorer

drop_files(V, Files) :-
	maplist(file_to_url, Files, URLs),
	debug(drop, '~p: Dropping files: ~p~n', [V, Files]),
	get(V, resource, R),
	catch(rdfe_transaction(inner::drop_file_resource(R, URLs),
			       individuals_from(Files)),
	      E, report_exception(V, E)).

file_to_url(File, URL) :-
	atom_concat('file://', File, URL).

drop_file_resource(R, URLs) :-
	forall(member(URL, URLs),
	       drop_resource(change_type, R, URL)).

:- end_rules.

:- begin_rules(rdf_click, default).

clicked(V) :-
	get(V, resource, R),
	format('Clicked ~p representing ~p~n', [V, R]).

:- end_rules.


:- begin_rules(rdf_tab, default).

:- pce_autoload(image_window, img_tab).

%	resource_tab(-Name, -Window)
%	
%	Create the windows that appear as tabs in the right-hand window
%	of the explorer.  The <-name of the window determines the label
%	in the tab.

resource_tab(class, Window) :-
	new(Window, table_window(class, new(rdf_class_sheet))).
resource_tab(instance, Window) :-
	new(Window, table_window(class, new(rdf_instance_sheet))).
resource_tab(triples, Window) :-
	new(Window, table_window(class, new(rdf_cached_triple_table))).
resource_tab(image, Window) :-
	new(Window, image_window).

%	default_resource_tab(+Resource, -Tab)
%	
%	Return the name of the tab that should be on top after selecting
%	Resource.

default_resource_tab(Resource, class) :-
	rdfs_individual_of(Resource, rdfs:'Class').
default_resource_tab(_, instance).
	
:- end_rules.

:- begin_rules(image_window, default).

image(Resource, Img) :-
	file_name_extension(_, Ext0, Resource),
	downcase_atom(Ext0, Ext),	% avoid DOS/Unix trouble
	image_extension(Ext),
	new(I, url_image(Resource)),
%	get(I, exists, @on),		% demand existence not implemented BJW
	new(Img, bitmap(I)).	        % dont scale BJW

image_extension(jpeg).
image_extension(jpg).
image_extension(gif).

:- end_rules.


:- begin_rules(search, default).

%	find(+String, +Domain, ?Properties, +Method, -Subject)
%	
%	Search for literal text in resources   belonging  to a specified
%	domain.  Searching  for  ns:string  searches    for  string  for
%	resources in the specified namespace.

find(String0, Domain, PlFields, TheHow, Subject) :-
	ns_string(String0, String, Prefix),
	(   rdf_current_dialect(rdfs)
	->  rdfs_find(String, Domain, PlFields, TheHow, Subject)
	;   owl_find(String, Domain, PlFields, TheHow, Subject)
	),
	sub_atom(Subject, 0, _, _, Prefix).

ns_string(String0, String, NS) :-
	concat_atom([NSid, String], :, String0),
	rdf_db:ns(NSid, NS), !.
ns_string(String, String, '').

:- end_rules.


		 /*******************************
		 *	      TOOL		*
		 *******************************/

:- begin_rules(rdfs_explorer, default).

show_triple_cache(Cache) :-
	get(@particle, self, Tool),
	get(Tool, member, rdf_sheet, Sheet),
	send(Sheet, triples, Cache).

view_owl_class_extension :-
	get(@particle, self, Explorer),
	send(Explorer, has_get_method, view_owl_class_extension),
	get(Explorer, view_owl_class_extension, @on).

view_inferred_super_properties :-
	get(@particle, self, Explorer),
	send(Explorer, has_get_method, view_inferred_super_properties),
	get(Explorer, view_inferred_super_properties, @on).

open_resource(Resource, How) :-
	get(@particle, self, Explorer),
	send(Explorer, has_send_method, open_resource),
	send(Explorer, open_resource, Resource, How).

:- end_rules.

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
		    rdf_predicate,
		    rdf_tab,
		    search,
		    rdfs_explorer
		  ]).
:- end_particle.


		 /*******************************
		 *	       LABELS		*
		 *******************************/

:- begin_rules(rdf_resource_id_text, default).

label_text(Resource, Text) :-
	rdf_global_id(NS:Local, Resource),
	concat_atom([NS, :, Local], Text).

:- end_rules.


		 /*******************************
		 *	     HIERARCHY		*
		 *******************************/

:- begin_rules(rdf_tree, default).

clicked(V) :-
	get(V, resource, R),
	get(V, container, rdf_node, Node),
	(   get(Node, resource, R)
	->  send(Node?tree, selected, Node)
	;   send(Node?tree, selected, V)
	).

menu_item(Group, Item) :-
	outer::menu_item(Group, Item).
menu_item(view, show_all_parents).
menu_item(edit, unrelate=unrelate_resource).
menu_item(edit, delete=delete_resource).
menu_item(edit, delete_class_hierarchy).

menu_item(Gr, edit, new(Role), Node) :-
	(   rdf_resource_menu:container_with_method(Gr, new, Node),
	    send(Node, instance_of, rdf_node)
	->  (   get(Node?caches, attribute_names, Roles),
	        chain_list(Roles, List),
		member(Role, List),
		Role \== rdf_orphan_node
	    ;	get(Node, resource, Resource),
		rdfs_subclass_of(Resource, rdfs:'Class'),
		\+ rdfs_subclass_of(Resource, owl:'Restriction'),
		Role = rdf_individual_node
	    )
	).
menu_item(Gr, Group, Item, Receiver) :-
	outer::menu_item(Gr, Group, Item, Receiver),
	Item \== hierarchy_location.

:- end_rules.

:- begin_rules(rdf_node, default).

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
	outer::drop(Command, Onto, From).

:- end_rules.


:- begin_rules(rdf_individual_node, default).

icon_resource(_, individual).
icon_resource(R, Icon) :-
	super::icon_resource(R, Icon),
	Icon \== individual.

:- end_rules.

:- begin_rules(rdf_part_node, default).

icon_resource(_, part).
icon_resource(R, Icon) :-
	super::icon_resource(R, Icon).

:- end_rules.

:- begin_rules(rdf_property_node, default).

child_cache(R, Cache, rdf_property_node) :-
	rdf_cache(lsorted(V), rdf_has(V, rdfs:subPropertyOf, R), Cache).

menu_item(Group, Item) :-
	outer::menu_item(Group, Item).
menu_item(view, triples=view_triples).

:- end_rules.

:- begin_rules(rdf_root_node, default).

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

:- end_rules.


:- begin_rules(rdf_orphan_node, default).

label_text('__orphan_classes', '<Classes without rdfs:subClassOf>').
label_text('__orphan_resources', '<Resources without rdf:type>').
%label_text(Resource, Label) :-
%	super::label_text(Resource, Label).

icon_resource('__orphan_classes', orphanclass) :- !.
icon_resource('__orphan_resources', orphanres) :- !.
icon_resource(Resource, Icon) :-
	super::icon_resource(Resource, Icon).

label_class('__orphan_classes', rdfs_class_label).
label_class('__orphan_resources', rdf_individual_label).
label_class(Resource, Class) :-
	super::label_class(Resource, Class).

child_cache('__orphan_classes', Cache, rdf_class_node) :-
	rdf_cache(lsorted(X), orphan_class(X), Cache).
child_cache('__orphan_resources', Cache, rdf_node) :-
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
	rdf(_, Resource, _),
	\+ rdf_has(Resource, rdf:type, _).
orphan_resource(Resource) :-
	rdf(_, _, Resource),
	atom(Resource),
	\+ rdf_has(Resource, rdf:type, _).

:- end_rules.


		 /*******************************
		 *	      TABLE		*
		 *******************************/

:- begin_rules(rdf_tabular, default).

clicked(V) :-
	send(V?device, selection, V).

:- end_rules.

:- begin_rules(rdf_object_cell, default).

menu_item(Group, Item) :-
	super::menu_item(Group, Item).
menu_item(edit, modify).
menu_item(edit, delete).

drop_command(_Me, _Resource, modify) :-
	true.				% must validate restrictions!

drop(modify, Gr, V) :-
	get(V, resource, Resource),
	get(Gr, triple, rdf(Subject, Predicate, Old)),
	rdf_set_object_or_anon_instance(Subject, Predicate, Old, Resource).

:- end_rules.


:- begin_rules(rdf_predicate_cell, default).

menu_item(Group, Item) :-
	super::menu_item(Group, Item).
menu_item(edit, add_value).
menu_item(edit, delete_all_values).

drop_resource_command(_Me, Resource, modify) :-
	rdfs_individual_of(Resource, rdf:'Property').
drop_resource_command(_Me, _Resource, add) :-
	true.				% must validate restrictions!

drop(add, Gr, V) :-
	get(V, resource, Resource),	
	get(Gr, triple, rdf(Subject, Predicate, _)),
	rdf_add_object(Subject, Predicate, Resource).
drop(modify, Gr, V) :-
	get(V, resource, NewP),	
	get(Gr, triple, rdf(S, P, _)),
	P \== NewP,
	rdfe_transaction(forall(rdf(S,P,O),
				rdfe_update(S, P, O, predicate(NewP)))).

:- end_rules.

:- begin_rules(rdf_range_cell, default).

drop(modify, _Gr, V) :-
	get(V, resource, Resource),
	get(@particle, triple, rdf(Subject, Predicate, Old)),
	rdf_set_object(Subject, Predicate, Old, Resource).

:- end_rules.


:- begin_rules(rdf_domain_cell, default).

drop(modify, _Gr, V) :-
	get(V, resource, Resource),
	get(@particle, triple, rdf(Subject, Predicate, Old)),
	rdf_set_object(Subject, Predicate, Old, Resource).

:- end_rules.


:- begin_rules(rdf_not_filled_label, default).

menu_item(edit, delete).
menu_item(edit, fill=modify).
menu_item(edit, literal_value=type(literal)).

clicked(V) :-
	send(V, modify).

:- end_rules.


		 /*******************************
		 *	      LISTS		*
		 *******************************/

:- begin_rules(rdf_list_label, default).

icon_resource(R, Icon) :-		% kill outer extra icons
	display:icon_resource(R, Icon).

menu_item(Group, Item) :-
	rdf_resource_menu:menu_item(Group, Item).
menu_item(edit, delete=delete_member(@arg1)). % @arg1 = popup object

drop_command(_Me, _Resource, append).
drop_command(_Me, _Resource, prepend).
drop_command(_Me, _Resource, modify).

drop(Action, Gr, V) :-
	get(V, resource, Resource),
	get(Gr, triple, Triple),
	debug(drop, 'Drop ~w: ~w on ~w~n', [Action, Resource, Triple]),
	rdf_list_operation(Action, Triple, Resource).

:- end_rules.


		 /*******************************
		 *	     OWL STUFF		*
		 *******************************/


:- begin_rules(owl_description_label, default).

menu_item(Group, Item) :-
	super::menu_item(Group, Item).
menu_item(type, oneOf=owl_description_type(P)) :-
	rdf_equal(P, owl:oneOf).
menu_item(type, complementOf=owl_description_type(P)) :-
	rdf_equal(P, owl:complementOf).
menu_item(type, unionOf=owl_description_type(P)) :-
	rdf_equal(P, owl:unionOf).
menu_item(type, intersectionOf=owl_description_type(P)) :-
	rdf_equal(P, owl:intersectionOf).

sub_menu(Popup) :-
	super::sub_menu(Popup).
sub_menu(type).

:- end_rules.

