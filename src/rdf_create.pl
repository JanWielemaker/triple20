/*  $Id$

    Developed in the MIA project
    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 2000 University of Amsterdam. All rights reserved.
*/

:- module(rdf_create, []).
:- use_module(library(pce)).
:- use_module(semweb(rdf_db)).
:- use_module(semweb(rdf_edit)).

:- pce_autoload(identifier_item, library(pce_identifier_item)).

:- pce_begin_class(rdf_create_dialog, dialog,
		   "Create instance or class").

variable(role,     name,    get, "Role of the new individual").
variable(resource, name,    get, "Context Class").
variable(client,   object*, get, "Object to report to").

initialise(D, Parent:name, Role:name, Client:[object]*) :->
	default(Client, @nil, Cntl),
	send(D, slot, role, Role),
	send(D, slot, resource, Parent),
	send(D, slot, client, Cntl),
	send_super(D, initialise, string('Create %s', Role?label_name)),
	rdf_global_id(NS:_, Parent),
	send(D, append, new(rdf_ns_menu(NS))),
	send(D, append, new(rdf_id_item), right),
	send(D, append, new(C, button(create, message(D, create_resource)))),
	send(D, append, button(done)),
	send(C, default_button, @on).

done(D) :->
	send(D, destroy).

cancel(D) :->
	send(D, done).

create_resource(D) :->
	"Create (new) resource from dialog contents"::
	get(D, member, namespace, NSI),
	get(NSI, selection, NS),
	get(D, member, id, IDI),
	get(IDI, selection, Label),
	local_uri_from_label(NS, Label, Local),
	atom_concat(NS, Local, Resource),
	rdfe_transaction(send(D, do_create_resource, Resource, Label),
			 create_resource),
	send(IDI, clear),
	(   get(D, client, Client),
	    send(Client, has_send_method, resource_created)
	->  get(D, role, Role),
	    send(Client, resource_created, Resource, Role)
	;   true
	).

do_create_resource(D, Resource:name, Label:name) :->
	"Create a new resource"::
	get(D, resource, Super),
	(   get(D, role, rdf_class_node)
	->  rdfe_assert(Resource, rdf:type, rdfs:'Class'),
	    rdfe_assert(Resource, rdfs:subClassOf, Super)
	;   rdfe_assert(Resource, rdf:type, Super)
	),
	rdfe_assert(Resource, rdfs:label, literal(Label)).

local_uri_from_label(_, Label, Local) :-
	new(S, string('%s', Label)),
	send(S, translate, ' ', '_'),
	get(S, value, Local),
	free(S).

:- pce_end_class(rdf_create_dialog).


:- pce_begin_class(rdf_ns_menu, menu,
		   "Prompt for namespace").

initialise(M, Default:[name], Msg:[code]*) :->
	send_super(M, initialise, namespace, cycle, Msg),
	findall(NS, rdf_db:ns(NS, _), List0),
	sort(List0, List),
	(   member(NS, List),
	    rdf_db:ns(NS, Full),
	    send(M, append, menu_item(Full, @default, NS)),
	    fail
	;   true
	),
	(   Default \== @default,
	    rdf_db:ns(Default, FullDefault)
	->  send(M, selection, FullDefault)
	;   true
	),
	send(M, show_label, @off).

:- pce_end_class(rdf_ns_menu).

:- pce_begin_class(rdf_id_item, identifier_item,
		   "Enter a local id").

initialise(ID, Default:[name]) :->
	send_super(ID, initialise, id, Default),
	send(ID, show_label, @off).

typed(Id, Ev:event) :->
	(   get(Ev, id, 27)
	->  send(Id?device, cancel) % hack!
	;   send_super(Id, typed, Ev)
	).

:- pce_end_class(rdf_id_item).
