/*  $Id$

    Developed in the MIA project
    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 2000 University of Amsterdam. All rights reserved.
*/

:- module(pce_rdfs_hierarchy, []).
:- use_module(library(pce)).
:- use_module(library(pce_report)).
:- use_module(library(hyper)).
:- use_module(rdf_explorer).
:- use_module(rdf_db).
:- use_module(rdfs).
:- use_module(owl).
:- use_module(rdf_edit).
:- use_module(library(broadcast)).
:- use_module(library(rdf_render)).

:- pce_autoload(rdfs_new_dialog, library(rdfs_new_item)).

:- pce_begin_class(rdfs_hierarchy, tree, "Show ontology hierarchy").

variable(domain,	 prolog,      get,  "Subdomain of the ontology").
variable(message,	 code*,	      both, "Message on select").
variable(open_message,	 code*,	      both, "Message  on double-click").
variable(selectable,	 chain*,      both, "Roles of selectable nodes").
variable(rdfs_root,	 name,	      get,  "Term serving as root").
variable(show_namespace, bool := @on, get,  "Show namespaces").

:- pce_global(@onto_tree_recogniser,
	      make_onto_tree_recogniser).

make_onto_tree_recogniser(R) :-
	new(R, handler_group),
	send(R, append, click_gesture(left, '', single,
				      message(@receiver, on_left_click))),
	send(R, append, new(KB, key_binding)),
	send(KB, function, '\\C-f', find).

initialise(OT,
	   Domain0:domain=[prolog]) :->
	"Open hierarchy browser on domain of ontology"::
	rdf_equal(rdfs:'Resource', RootID),
	send(OT, slot, rdfs_root, RootID),
	rdf_global_term(Domain0, Domain),
	send(OT, domain, Domain),
	send_super(OT, initialise, new(Root, rdfs_node(RootID, @nil, OT))),
	send(OT, direction, list),
	send(OT, level_gap, 20),
	send(OT, selectable, chain(class, domain, instance)),
	send(Root, update_expandable),
%	send(OT, show_domain),
	listen(OT, rdf_transaction(_), send(OT, refresh)).

unlink(OT) :->
	unlisten(OT),
	send_super(OT, unlink).


:- pce_group(settings).

show_namespace(OT, Show:bool) :->
	"Show/hide namespace ID"::
	(   get(OT, show_namespace, Show)
	->  true
	;   send(OT, slot, show_namespace, Show),
	    send(OT?root, for_all, message(@arg1, update_label))
	).

:- pce_group(event).

event(OT, Ev:event) :->
	"Deal with events"::
	(   send_super(OT, event, Ev)
	;   send(@onto_tree_recogniser, event, Ev)
	).

on_left_click(OT) :->
	"Deselect all nodes"::
	send(OT, selection, @nil).

selected(OT, Node:rdfs_node) :->
	"User selected a node"::
	send(OT, selection, Node?image),
	(   get(OT, message, M),
	    M \== @nil
	->  get(Node, id, Term),
	    send(M, forward, Term)	% @arg1 = term
	;   true
	).

open_node(OT, Node:rdfs_node) :->
	"User double-clicked a node"::
	(   get(OT, open_message, M),
	    M \== @nil
	->  get(Node, id, Term),
	    send(M, forward, Term)	% @arg1 = term
	;   true
	).


:- pce_group(build).

member(OT, Id:name, Node:rdfs_node) :<-
	"Find Node representing ID"::
	get(OT?root, find, @arg1?name == Id, Node).

add(OT, Id:name) :->
	"Add node for Id to the tree, as well as the path"::
	get(OT, add, Id, _Node).

add(OT, Id:name, Below:[name], Node:rdfs_node) :<-
	"Add node for Id to the tree, as well as the path"::
	(   Below == @default
	->  (   get(OT, member, Id, Node)
	    ->	true
	    ;	get(OT, rdfs_root, Root),
		get(OT, add, Id, Root, Node)
	    )
	;   findall(P, path(Id, Below, P), Paths),
	    pp(Id=Paths),
	    (   Paths = [First|Tail]
	    ->  add_path(First, OT, Node),
		add_paths(Tail, OT)
	    ;   rdf_render_parent_relation(Id, rdfs_hierarchy, Pred),
		rdf_has(Id, Pred, Super)
	    ->  get(OT, add, Super, SuperNode),
		send_class(Super, node, collapsed(@off)),
		send(SuperNode, son, new(Node, rdfs_node(Id, Pred, OT)))
	    ;   get(OT, root, Root),
		send_class(Root, node, collapsed(@off)),
		send(Root, son, new(Node, rdfs_node(Id, @nil, OT)))
	    )
	).
	    
add_paths([], _).
add_paths([H|T], OT) :-
	add_path(H, OT, _),
	add_paths(T, OT).

add_path([H], OT, Node) :-
	get(OT, member, H, Node), !.
add_path([Pred-H1,H2|T], OT, Node) :-
	(   get(OT, member, H1, Node)
	->  true
	;   add_path([H2|T], OT, Node2),
	    new(Node, rdfs_node(H1, Pred, Node2)),
	    send_class(Node2, node, collapsed(@off)),
	    send(Node2, son, Node)
	).

path(Node, Node, [Node]).
path(Node, Root, [Pred-Node|Path]) :-
	rdf_render_parent_relation(Node, rdfs_hierarchy, Pred),
	rdf_has(Node, Pred, Parent),
	path(Parent, Root, Path).

node_label(OT, Id:name, Label:name) :<-
	"Get label to display for Id"::
	(   get(OT, show_namespace, @off)
	->  rdfs_label(Id, Label)
	;   rdfs_ns_label(Id, Label)
	).

:- pce_group(domain).

domain(OT, Domain:[prolog]) :->
	"Define the domain (OWL description)"::
	(   Domain == @default
	->  get(OT, rdfs_root, Root),
	    send(OT, slot, domain,
		 union_of([ all_values_from(Root),
			    class(Root)
			  ]))
	;   atom(Domain)
	->  send(OT, slot, domain,
		 union_of([ all_values_from(Domain),
			    class(Domain)
			  ]))
	;   send(OT, slot, domain, Domain)
	).

add_domain_node(OT, Id:name) :->
	"Add a domain-node to the tree"::
	get(OT, add, Id, Node),
	fix_roles_in_path(Node, domain).

fix_roles_in_path(Node, Role) :-
	(   get(Node, role, Role)
	->  true
	;   send(Node, role, Role),
	    (	Role == abstract_domain
	    ->	send(Node, collapsed, @nil)
	    ;	true
	    ),
	    (   get(Node, parents, Parents),
		Parents \== @nil
	    ->  send(Parents, for_all, message(@prolog, fix_roles_in_path,
					       @arg1, abstract_domain))
	    ;	true
	    )
	).

show_domain(OT) :->
	"Show all nodes from the <-domain"::
	(   get(OT, root, Node),
	    Node \== @nil
	->  send(Node, collapsed, @on)
	;   true
	),
	get(OT, domain, Domain),
	forall(domain_node(Domain, Node),
	       send(OT, add_domain_node(Node))).

domain_node([H|T], Node) :-
	(   domain_node(H, Node)
	;   domain_node(T, Node)
	).
domain_node(class(Node), Node).
domain_node(union_of(Domains), Node) :-
	domain_node(Domains, Node).
domain_node(intersection_of(Domains), Node) :-
	domain_node(Domains, Node).
domain_node(one_of(Individuals), Node) :-
	member(Node, Individuals).
domain_node(all_values_from(Class), Class).
domain_node(some_values_from(Class), Class).


expand_domain(OT) :->
	"Expand all domain-nodes"::
	get(OT, domain, Domain),
	(   domain_node(Domain, Node),
	    send(OT, expand_node(Node)),
	    fail
	;   true
	).

collapse_domain(OT) :->
	"Collapse all domain-nodes"::
	send(OT?window, scroll_to, point(0,0)),
	get(OT, domain, Domain),
	(   domain_node(Domain, Node),
	    send(OT, collapse_node(Node)),
	    fail
	;   true
	).

expand_node(OT, Id:name) :->
	"Expand given node"::
	get(OT, add, Id, Node),
	(   get(Node, collapsed, @on)
	->  send(Node, collapsed, @off)
	;   true
	).
	
collapse_node(OT, Id:name) :->
	"Expand given node"::
	get(OT, member, Id, Node),
	(   get(Node, collapsed, @off)
	->  send(Node, collapsed, @on)
	;   true
	).

refresh(OT) :->
	"Refresh after change to the ontology"::
	send(OT?root, for_all, message(@arg1, refresh)).

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


:- pce_group(diagram).

open_diagram(OT, Id:name) :->
	"Open triple diagram from resource Id"::
	get(OT, rdf_diagram, Diagram),
	send(Diagram, resource, Id),
	send(Diagram, expose).

rdf_diagram(OT, Diagram:rdf_explorer) :<-
	"Get associated RDF explorer"::
	(   get(OT, hypered, rdf_explorer, Diagram)
	->  true
	;   new(Diagram, rdf_explorer),
	    new(_, partof_hyper(OT, Diagram, rdf_explorer, hierarchy))
	).

:- pce_end_class(rdfs_hierarchy).


:- pce_begin_class(rdfs_node(id), node, "Node in ontology hierarchy").

variable(id,	    name, get, "Represented resource").
variable(role, {abstract_domain,abstract,class,domain,indiviual} := class,
	 get, "Role of the class").
variable(relation,  name*,     get, "Relation to my parent").

role(N, Role:{abstract_domain,abstract,class,domain,indiviual}) :->
	"Set the role of the node"::
	send(N, slot, role, Role),
	node_attributes(Role, Atts),
	send_list(N, Atts).

node_attributes(abstract_domain, [ colour(blue),
				   font(normal)
				 ]).
node_attributes(domain,		 [ colour(blue),
				   font(bold),
				   underline(@on)
				 ]).
node_attributes(abstract,	 [ colour(grey50),
				   font(bold)
				 ]).
node_attributes(indiviual,	 [ colour(forestgreen),
				   font(italic)
				 ]).
node_attributes(class,		 []).

initialise(N, Id:name, Relation:name*, Parent:'rdfs_node|rdfs_hierarchy') :->
	"Create from resource and hierarchy"::
	send(N, slot, id, Id),
	send(N, slot, relation, Relation),
	(   send(Parent, instance_of, rdfs_hierarchy)
	->  Tree = Parent
	;   get(Parent, tree, Tree)
	),
	get(Tree, node_label, Id, Label),
	send_super(N, initialise, text(Label)),
	send(N, collapsed, @nil),
	send(N, name, Id),
	(   rdf_equal(Relation, rdf:type)
	->  send(N, role, indiviual)
	;   true
	).


update_label(N) :->
	"Update label after a global change"::
	get(N, id, Id),
	get(N?tree, node_label, Id, Label),
	send(N?image, string, Label).


update_expandable(N) :->
	"Update [+]/[-]/nothing"::
	(   send(N, has_sons)
	->  (   get(N, collapsed, @nil)
	    ->	send(N, collapsed, @on)
	    ;	true
	    )
	;   send(N, collapsed, @nil)
	).

has_sons(N) :->
	"See whether there are instance or other sons"::
	get(N, id, Resource),
	get(N?tree, domain, Domain),
	rdf_render_child_relation(Resource, rdfs_hierarchy, Rel),
	rdf_has(Son, Rel, Resource),
	owl_satisfies(Domain, Son), !.

collapsed(N, Val:bool*) :->
	"Toggle collapsed state"::
	(   Val == @on
	->  send(N?sons, for_all, message(@arg1, delete_tree))
	;   Val == @off
	->  send(N, expand)
	;   true
	),
	send_super(N, collapsed, Val).

expand(N) :->
	"Expand the hierarchy one level"::
	get(N, id, Resource),
	(   rdf_render_child_relation(Resource, rdfs_hierarchy, Rel),
	    send(N, show_sons, Rel),
	    fail
	;   true
	).

show_more(N, Rel:name, More:int) :->
	"Show some more on this relation"::
	get(N, shown, Rel, N0),
	NewMax is N0 + More,
	send(N, show_sons, Rel, @default, NewMax).

shown(N, Rel:name, Count:int) :<-
	"Count nodes visible attributed to relation"::
	new(C, number(0)),
	send(N?sons, for_all,
	     if(and(@arg1?relation == Rel,
		    not(message(@arg1, instance_of, rdfs_more_node))),
		message(C, plus, 1))),
	get(C, value, Count).

show_sons(N, Rel:name, Using:[class], Max:[int]) :->
	"Show sons that have a relation Rel with me"::
	default(Max, 10, MaxCount),
	get(N, tree, Tree),
	get(Tree, domain, Domain),
	get(N, id, Me),
	findall(Sub, child(Me, Rel, Domain, Sub), Set0),
	sort(Set0, Set),
	get(N, sons, Sons),
	move_to_rel(Sons, Rel),
	update_sons(N, Sons, Set, MaxCount, Rel, Using).

%	child(+Resource, +Relation, +Domain, -Label-Son)
%	
%	Determine my childs on the given relation that are within the
%	given OWL description.

child(Me, Rel, Domain, Label-Sub) :-
	rdf_has(Sub, Rel, Me),
	owl_satisfies(Domain, Sub),
	rdfs_label(Sub, Label).

%	move_to_rel(+SonsChain, +Relation)
%	
%	Set the chain's <-current to the first node on the given
%	relation.  If there is no such node, current is not set.

move_to_rel(Sons, Rel) :-
	(   send(Sons, current_no, 1)
	->  find_rel(Sons, Rel)
	;   true			% chain is empty
	).
	
find_rel(Sons, Rel) :-
	(   get(Sons, current, Node)
	->  (   get(Node, relation, Rel)
	    ->	true
	    ;	get(Sons, next, _),
	        find_rel(Sons, Rel)
	    )
	;   true			% end of chain
	).

update_sons(_N, Sons, [], _Left, Relation, _Class) :- !,
	delete_on_relation(Sons, Relation).
update_sons(N, Sons, _, 0, Relation, _Class) :- !,
	(   get(Sons, current, Current)
	->  (   get(Current, relation, Relation),
	        send(Current, instance_of, rdfs_more_node)
	    ->	true
	    ;	send(N, son, rdfs_more_node(Relation), Current)
	    )
	;   send(N, son, rdfs_more_node(Relation))
	).
update_sons(N, Sons, [_Label-Resource|T], Left, Rel, Class) :-
	get(Sons, current, Current),
	get(Current, id, Resource), !,
	get(Sons, next, _),
	Left1 is Left - 1,
	update_sons(N, Sons, T, Left1, Rel, Class).
update_sons(N, Sons, [Label-Resource|T], Left, Rel, Class) :-
	get(Sons, current, Current), !,
	(   get(Current, relation, Rel),
	    \+ send(Current, instance_of, rdfs_more_node)
	->  get(Current, id, CurrentResource),
	    rdfs_label(CurrentResource, CurrentLabel),
	    (   CurrentLabel @< Label		% Node was deleted
	    ->  get(Sons, next, _),
		send(Current, delete_tree),
		update_sons(N, Sons, [Label-Resource|T], Left, Rel, Class)
	    ;   get(N, create_son, Resource, Rel, Class, Son),
		send(N, son, Son, Current),	% insert before Current
		Left1 is Left - 1,
		update_sons(N, Sons, T, Left1, Rel, Class)
	    )
	;   get(N, create_son, Resource, Rel, Class, Son),
	    send(N, son, Son, Current),		% insert before Current
	    Left1 is Left - 1,
	    update_sons(N, Sons, T, Left1, Rel, Class)
	).
update_sons(N, Sons, [_Label-Resource|T], Left, Rel, Class) :-
	get(N, create_son, Resource, Rel, Class, Son),
	send(N, son, Son),
	Left1 is Left - 1,
	update_sons(N, Sons, T, Left1, Rel, Class).

%	delete_on_relation(+Sons, +Relation)
%	
%	Delete all remaining sons that are linked using the same
%	relation.

delete_on_relation(Sons, Relation) :-
	(   get(Sons, next, Current),
	    get(Current, relation, Relation)
	->  send(Current, delete_tree),
	    delete_on_relation(Sons, Relation)
	;   true
	).


create_son(N, Resource:name, Relation:name, _Class:[class], Son:rdfs_node) :<-
	"Create a child-node"::
	new(Son, rdfs_node(Resource, Relation, N)).

son(N, Son:node, Before:[node]*) :->
	send_super(N, son, Son, Before),
	send(Son, update_expandable).

refresh(N) :->
	"Ontology changed; check myself"::
	send(N, update_label),
	send(N, update_expandable),
	(   get(N, collapsed, @off)
	->  send(N, expand)
	;   send(N?sons, for_all, message(@arg1, delete_tree))
	).

:- pce_group(event).

:- free(@onto_node_recogniser).
:- pce_global(@onto_node_recogniser, make_onto_node_recogniser).
:- pce_global(@onto_node_popop, make_onto_node_popup).

make_onto_node_popup(Popup) :-
	Node = @arg1,
	new(Popup, popup(options)),
	send_list(Popup, append,
		  [ menu_item(show_id,
			      message(Node, report, inform, Node?id)),
		    menu_item(copy_id_to_clipboard,
			      message(Node, copy)),
		    menu_item(copy_as_xml_identifier,
			      message(Node, copy, xml_identifier)),
		    menu_item(copy_as_xml_attribute,
			      message(Node, copy, xml_attribute)),
		    menu_item(view_rdf_source,
			      message(Node, view_rdf_source)),
		    menu_item(diagram_,
			      message(Node?tree, open_diagram, Node?id)),
		    gap,
		    menu_item(delete,
			      message(Node, delete_resource))
		  ]).


make_onto_node_recogniser(G) :-
	new(P, popup_gesture(@receiver?popup)),
	new(C1, click_gesture(left, '', single,
			      message(@receiver,
				      on_left_click))),
			      
	new(C2, click_gesture(left, '', double,
			      message(@receiver,
				      on_double_left_click))),
	new(G, handler_group(P, C1, C2)).
			      

popup(_, Popup:popup) :<-
	Popup = @onto_node_popop.

event(N, Ev:event) :->
	"Handle node-event"::
	(   send_super(N, event, Ev)
	->  true
	;   send(Ev, post, N?image, @onto_node_recogniser)
	).

on_left_click(N) :->
	"Select the current node"::
	send(N?tree, selected, N).

on_double_left_click(N) :->
	"Select the current node"::
	get(N, role, Role),
	get(N?tree, selectable, Roles),
	(   (	Roles == @nil
	    ;	send(Roles, member, Role)
	    )
	->  send(N?tree, open_node, N)
	;   send(N, report, warning, 'Cannot open %s class', Role)
	).

copy(N, As:[{resource,xml_identifier,xml_attribute}]) :->
	"Copy resource to clipboard"::
	get(N, id, Resource),
	(   As == xml_identifier
	->  rdf_global_id(NS:Local, Resource),
	    new(Copy, string('%s:%s', NS, Local))
	;   As == xml_attribute
	->  rdf_global_id(NS:Local, Resource),
	    new(Copy, string('&%s;%s', NS, Local))
	;   Copy = Resource
	),
	send(@display, copy, Copy).

view_rdf_source(N) :->
	"Open Prolog editor on RDF source"::
	get(N, id, Id),
	(   rdf_source_location(Id, File:Line)
	->  edit(file(File, line(Line)))
	;   send(N, report, warning, 'Cannot find source for %s', Id)
	).

delete_resource(N) :->
	"Delete the underlying resource"::
	get(N, id, Id),
	findall(Triple, dependent_rdf(Id, Triple), Dep),
	(   Dep == []
	->  send(@display, confirm, 'Delete resource %s', Id)
	;   send(N, confirm_delete, Id, Dep)
	),
	rdfe_transaction(rdfe_delete(Id)).

dependent_rdf(Id, rdf(S,Id,O)) :-
	rdf(S, Id, O).
dependent_rdf(Id, rdf(S,P,Id)) :-
	rdf(S, P, Id).

confirm_delete(N, Id:name, Deps:prolog) :->
	"Confirm deletion of referred resource"::
	new(D, dialog('Confirm delete')),
	send(D, append,
	     text(string('The resource %s appears as object\n\
	                  in the following triples', Id),
		  left, normal)),
	send(D, append, new(T, rdf_triple_table(Deps))),
	send(D, resize_message,
	     message(T, table_width, @arg2?width - 2*D?border?width)),
	send(D, append,
	     text('WARNING: If you confirm these triples are removed',
		  left, bold)),
	send(D, append, button(ok, message(D, return, ok))),
	send(D, append, button(cancel, message(D, return, cancel))),
	send(D, transient_for, N?frame),
	send(D, modal, transient),
	get(D, confirm_centered, N?frame?area?center, OkCancel),
	send(D, destroy),
	OkCancel == ok.

:- pce_end_class(rdfs_node).


:- pce_begin_class(rdfs_class_node, rdfs_node,
		   "Node showing a class").

initialise(N, Id:name, Tree:rdfs_hierarchy) :->
	send(N, slot, id, Id),
	get(Tree, node_label, Id, Label),
	Name = Id,
	(   rdf_has(_Child, rdfs:subClassOf, Id)
	->  send_super(N, initialise, new(T, text(Label, left, bold))),
	    send_super(N, collapsed, @on),
	    (   fail		% send(O, abstract, Id)
	    ->  send(T, colour, grey50),
		send(N, slot, role, abstract)
	    ;   true
	    )
	;   get(Tree, domain, Domain),
	    send(N, has_sons, Domain)
	->  send_super(N, initialise, new(T, text(Label, left, normal))),
	    send_super(N, collapsed, @on)
	;   send_super(N, initialise, new(T, text(Label, left, normal))),
	    send_super(N, collapsed, @nil)
	),
	send(T, name, Name).

:- free(@rdfs_class_node_popop).
:- pce_global(@rdfs_class_node_popop, make_rdfs_class_node_popup).

make_rdfs_class_node_popup(Popup) :-
	Node = @arg1,
	new(Popup, popup(options)),
	send_list(Popup, append,
		  [ menu_item(new_individual,
			      message(Node, new_individual)),
		    gap,
		    menu_item(show_id,
			      message(Node, report, inform, Node?id)),
		    menu_item(copy_id_to_clipboard,
			      message(Node, copy)),
		    menu_item(copy_as_xml_identifier,
			      message(Node, copy, xml_identifier)),
		    menu_item(copy_as_xml_attribute,
			      message(Node, copy, xml_attribute)),
		    menu_item(view_rdf_source,
			      message(Node, view_rdf_source)),
		    menu_item(diagram_,
			      message(Node?tree, open_diagram, Node?id)),
		    gap,
		    menu_item(delete,
			      message(Node, delete_resource))
		  ]).

popup(_, Popup:popup) :<-
	Popup = @rdfs_class_node_popop.

new_individual(N) :->
	"Create a new indivisual"::
	get(N, id, Class),
	new(D, rdfs_new_dialog(Class)),
	get(N, display_position, point(X, Y)),
	send(D, transient_for, N?frame),
	send(D, modal, transient),
	get(D, confirm, point(X, Y+20), Individual),
	send(D, destroy),
	get(N, tree, Hierarchy),
	get(Hierarchy, add, Individual, Node),
	send(N?tree, selected, Node).

:- pce_end_class(rdfs_class_node).


:- pce_begin_class(rdfs_orphan_class_node, rdfs_class_node,
		   "Abstract node for orphan classes").

initialise(Node) :->
	"Create handle for orphaned classes"::
	send(Node, slot, id, '__orphan_classes'),
	send(Node, name, '__orphan_classes'),
	send_class(Node, node,
		   initialise(new(T, text('<Orphan Classes>')))),
	send_class(Node, node, collapsed(@on)),
	send(T, colour, red).

expand(Node, Max:[int]) :->
	"Show orphan classes"::
	default(Max, 10, MaxCount),
	flag(count, Old, 1),
	(   orphan_class(Class),
	    \+ get(Node?sons, find, @arg1?id == Class, _),
	    flag(count, C, C+1),
	    (   C > MaxCount
	    ->  !, send(Node, more)
	    ;   send(Node, sub_class, Class),
		fail
	    )
	;   flag(count, _, Old)
	).

show_more(N, Max:[int]) :->
	"Handle counts from more"::
	send(N, expand, Max).

update_label(_) :->
	true.				% I've got a fixed label

refresh(Node) :->
	"Update myself"::
	(   orphan_class(_)
	->  true
	;   send(Node, delete_tree)	% no more left; delete myself
	).

orphan_class(Class) :-
	rdfs_individual_of(Class, rdfs:'Class'),
	\+ rdf_has(Class, rdfs:subClassOf, _),
	\+ rdf_equal(Class, rdfs:'Resource').

popup(_, _Popup:popup) :<-
	"Nothing to do here"::
	fail.

:- pce_end_class(rdfs_orphan_class_node).



:- pce_begin_class(rdfs_individual_node, rdfs_node,
		   "Node showing an individual").

initialise(N, Id:name, Tree:rdfs_hierarchy) :->
	send(N, slot, id, Id),
	get(Tree, node_label, Id, Label),
	send_super(N, initialise, new(T, text(Label, left, normal))),
	send(N, role, instance),
	atom_concat('$I$', Id, Name),
	send(T, name, Name),
	(   send(N, has_sons)
	->  send_class(N, node, collapsed(@on))
	;   send(N, collapsed, @nil)
	).

has_sons(N) :->
	"I can expand if I have subPropertyOf"::
	get(N, id, Id),
	rdf_has(_, rdfs:subPropertyOf, Id), !.

expand(N) :->
	"Show sub-properties"::
	get(N, id, Id),
	get(N, tree, OT),
	(   rdf_has(Sub, rdfs:subPropertyOf, Id),
	    \+ get(N?sons, find, @arg1?id == Sub, _),
	    send(N, son, rdfs_individual_node(Sub, OT)),
	    fail
	;   true
	).

:- pce_end_class(rdfs_individual_node).


:- pce_begin_class(rdfs_orphan_individual_node, rdfs_class_node,
		   "Add individuals without class here").

variable(sort_key, name := z, both, "Key for sorting node-types"). % added BJW

initialise(Node) :->
	"Create handle for orphaned classes"::
	send(Node, slot, id, '__orphan_instances'),
	send(Node, name, '__orphan_instances'),
	send_class(Node, node,
		   initialise(new(T, text('<Orphan Individuals>')))),
	send_class(Node, node, collapsed(@on)),
	send(T, colour, red).

expand(Node, Max:[int]) :->
	"Show orphan individuals"::
	default(Max, 10, MaxCount),
	flag(count, Old, 1),
	(   orphan_individual(Individual),
	    \+ get(Node?sons, find, @arg1?id == Individual, _),
	    flag(count, C, C+1),
	    (   C > MaxCount
	    ->  !, send(Node, more)
	    ;   send(Node, sub_instance, Individual),
		fail
	    )
	;   flag(count, _, Old)
	).

show_more(N, Max:[int]) :->
	"Handle counts from more"::
	send(N, expand, Max).

refresh(Node) :->
	"Update myself"::
	(   orphan_individual(_)
	->  true
	;   send(Node, delete_tree)	% no more left; delete myself
	).

orphan_individual(Individual) :-
	rdf_subject(Individual),
	\+ rdf_has(Individual, rdf:type, _).

:- pce_end_class(rdfs_orphan_individual_node).


:- pce_begin_class(rdfs_more_node, node,
		   "Show more alternatives").

variable(role,	   name := instance, both, "Role I play").
variable(relation, name, get, "Relation to expand further").

initialise(N, Relation:name) :->
	"Create `more' button"::
	send_super(N, initialise,
		   new(D, figure)),
	send(N, slot, relation, Relation),
	send(D, pen, 1),
	send(D, border, 2),
	send(D, format, new(Fmt, format(vertical, 1, @on))),
	send(Fmt, adjustment, vector(center)),
	send_list(D, display,
		  [ text('Next', left, bold),
		    new(B0, small_button(10,   message(N, more, 10))),
		    new(B1, small_button(100,  message(N, more, 100))),
		    new(B2, small_button(1000, message(N, more, 1000)))
		  ]),
	send_list([B0,B1,B2], show_focus_border(@off)),
	send(N, collapsed, @nil).

update_label(_) :->
	"Dummy"::
	true.

update_expandable(_) :->
	"Dummy"::
	true.

refresh(_) :->
	"Dummy"::
	true.

compare(_N, _To:node, Diff:{smaller,equal,larger}) :<-
	"Dummy to stay at the bottom"::
	Diff = larger.

more(N, More:[int]) :->
	"Show N more childs on this relation"::
	get(N, parents, chain(Parent)),
	get(N, relation, Relation),
	send(N, free),
	send(Parent, show_more, Relation, More).

:- pce_end_class(rdfs_more_node).


		 /*******************************
		 *	    SMALL BUTTON	*
		 *******************************/

:- pce_begin_class(small_button, button,
		   "Button with minimal size").

class_variable(size, size, size(5,5)).

:- pce_end_class(small_button).
