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

:- module(rdf_explorer, []).
:- use_module(library(pce)).
:- use_module(library(persistent_frame)).
:- use_module(library(toolbar)).
:- use_module(library(pce_report)).
:- use_module(library(rdf_diagram)).
:- use_module(semweb(rdf_db)).
:- use_module(semweb(rdfs)).

:- pce_begin_class(rdf_explorer, persistent_frame,
		   "Explore RDF relations").

:- pce_group(build).

initialise(Explorer) :->
	send_super(Explorer, initialise, 'RDF relation explorer'),
	send(Explorer, append, new(TD, tool_dialog(Explorer))),
	send(new(D, rdf_diagram_editor), below, TD),
	send(D, name, diagram),
	send(new(report_dialog), below, D),
	send(Explorer, fill_tool_dialog, TD).

fill_tool_dialog(Explorer, TD:tool_dialog) :->
	get(Explorer, diagram, Diagram),
	send(TD, append, new(File, popup(file))),
	send(TD, append, new(View, popup(view))),
	send_list(File, append,
		  [ menu_item(exit, message(Explorer, destroy))
		  ]),
	send(View, multiple_selection, @on),
	send(View, show_current, @on),
	send_list(View, append,
		  [ menu_item(show_namespace,
			      message(Diagram, show_namespace, @arg1)),
		    menu_item(type_in_node,
			      message(Diagram, type_in_node, @arg1)),
		    gap,
		    menu_item(auto_layout,
			      message(Diagram, auto_layout, @arg1))
		  ]),
	send(View, selected, show_namespace, @on),
	send(View, selected, type_in_node, @on),
	send(View, selected, auto_layout, @on).

diagram(Explorer, D:rdf_diagram) :<-
	"Get the drawing window"::
	get(Explorer, member, diagram, D).

resource(Explorer, Resource:name) :->
	"Add a resource"::
	send(Explorer?diagram, resource, Resource).

:- pce_end_class(rdf_explorer).


		 /*******************************
		 *	      WINDOW		*
		 *******************************/

:- pce_begin_class(rdf_diagram_editor, rdf_diagram,
		   "Edit-capable RDF handler").

variable(show_namespace, bool := @on, both, "Show/do not show namespace").

fill_popup(D) :->
	send(D, popup, new(P, popup)),
	send_list(P, append,
		  [ menu_item(layout, message(D, layout)),
		    gap,
		    menu_item(print, message(D, print)),
		    gap,
		    menu_item(clear, message(D, clear, destroy))
		  ]).


resource(E, Resource:name) :->
	"Append resource and some relations"::
	(   get(E, resource, Resource, @off, _)
	->  true
	;   get(E, resource, Resource, @on, Subject),
	    (	get(E, type_in_node, @on)
	    ->  send(Subject, show_types)
	    ;	true
	    ),
	    send(Subject, connect_to_existing_subjects)
	).
	    
create_resource(E, Resource:name, Subject:rdf_editable_resource) :<-
	new(Subject, rdf_editable_resource(Resource, E)).
	
create_literal(_E, Value:prolog, Subject:rdf_editable_literal) :<-
	new(Subject, rdf_editable_literal(Value)).
	
node(E, Node:prolog) :->
	"Add a node"::
	(   atom(Node)
	->  send(E, resource, Node)
	;   get(E, literal, Node, Gr),
	    send(Gr, connect_to_existing_subjects)
	).

expand_relation(E, Pred:name) :->
	"Test whether this relation must be used for expansion"::
	(   get(E, type_in_node, @off)
	->  true
	;   \+ rdfs_subproperty_of(Pred, rdf:type)
	).
%	\+ rdfs_subproperty_of(Pred, rdfs:label).

:- pce_end_class(rdf_diagram_editor).


		 /*******************************
		 *	     OBJECTS		*
		 *******************************/

:- pce_begin_class(rdf_editable_resource, rdf_resource,
		   "Extended version of the resource").

popup(R, P:popup) :<-
	"Editable popup"::
	new(P, popup),
	send_list(P, append,
		  [ menu_item(copy_resource,
			      message(R, copy_resource)),
		    gap,
		    menu_item(expand_subject,
			      message(R, expand_subject)),
		    menu_item(expand_object,
			      message(R, expand_object)),
		    menu_item(expand,
			      message(R, expand)),
		    menu_item(quit,
			      message(R, destroy)),
		    menu_item(keep_only_me,
			      message(R, clear_but_me)),
		    gap,
		    menu_item(layout,
			      message(R, layout))
		  ]).
			      

expand_subject(R) :->
	"Show related objects (me is Subject)"::
	get(R, device, Window),
	get(R, name, Id),
	(   rdf(Id, Predicate, Subject),
	    send(Window, expand_relation, Predicate),
	    send(Window, node, Subject),
	    fail
	;   true
	).

expand_object(R) :->
	"Show related subjects (me is Object)"::
	get(R, device, Window),
	get(R, name, Id),
	(   rdf(Subject, Predicate, Id),
	    send(Window, expand_relation, Predicate),
	    send(Window, node, Subject),
	    fail
	;   true
	).

expand(R) :->
	"Show me as source and target of relations"::
	send(R, expand_subject),
	send(R, expand_object).


clear_but_me(R) :->
	"Remove everything but me"::
	get(R, device, Device),
	send(Device?graphicals, for_all,
	     if(and(@arg1 \== R,
		    message(@arg1, instance_of, rdf_any)),
		message(@arg1, destroy))).

copy_resource(R) :->
	"Set resource on clipboard"::
	get(R, name, Name),
	send(@display, copy, Name).

show_types(R) :->
	"Show types I belong too"::
	get(R, name, Id),
	forall(rdf_has(Id, rdf:type, Type),
	       send(R, type, Type)).

connect_to_existing_subjects(R) :->
	"Create connections to all related subjects"::
	get(R, device, Window),
	send(Window?graphicals, for_all,
	     if(and(message(@arg1, instance_of, rdf_any),
		    @arg1 \== R),
		message(R, show_connections_to, @arg1))).

show_connections_to(R, Subject:rdf_any) :->
	"Show existing predicates between me and Subject"::
	get(R, name, Me),
	(   send(Subject, instance_of, rdf_resource)
	->  get(Subject, name, He),
	    forall(rdf(Me, Predicate, He),
		   send(R, connect, Predicate, Subject)),
	    forall(rdf(He, Predicate, Me),
		   send(Subject, connect, Predicate, R))
	;   send(Subject, instance_of, rdf_literal)
	->  get(Subject, value, Literal),
	    forall(rdf(Me, Predicate, Literal),
		   send(R, connect, Predicate, Subject))
	).


:- pce_end_class(rdf_editable_resource).


:- pce_begin_class(rdf_editable_literal, rdf_literal,
		   "Literal with advanced features").

popup(R, P:popup) :<-
	"Editable popup"::
	new(P, popup),
	send_list(P, append,
		  [ menu_item(quit,
			      message(R, destroy)),
		    gap,
		    menu_item(layout,
			      message(R, layout))
		  ]).

connect_to_existing_subjects(R) :->
	"Create connections to all related subjects"::
	get(R, device, Window),
	send(Window?graphicals, for_all,
	     if(and(message(@arg1, instance_of, rdf_resource),
		    @arg1 \== R),
		message(R, show_connections_to, @arg1))).


show_connections_to(R, To:rdf_resource) :->
	"Show existing predicates between me and Subject"::
	get(R, value, Value),
	get(To, name, Subject),
	forall(rdf(Subject, Predicate, Value),
	       send(To, connect, Predicate, R)).


:- pce_end_class(rdf_editable_literal).
