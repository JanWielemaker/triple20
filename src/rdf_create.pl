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


:- module(rdf_create, []).
:- use_module(library(pce)).
:- use_module(semweb(rdf_db)).
:- use_module(semweb(rdf_edit)).
:- use_module(library(lists)).
:- use_module(rdf_util).

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
	rdf_default_file(Parent, File, NS),
	send(D, append, new(rdf_file_menu(File))),
	send(D, append, new(rdf_ns_menu(NS))), % TBD: update if file changes
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
	get(NSI, selection, NSId),
	rdf_db:ns(NSId, NS),
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
	(   get(D, member, file, FileItem),
	    get(FileItem, selection, File)
	->  true
	;   File = user
	),
	(   get(D, role, rdf_class_node) 		% TBD: generalise!
	->  (   (   rdf_has(Super, rdf:type, MetaClass, Type)
		*-> rdfe_assert(Resource, Type, MetaClass, File)
		;   rdfe_assert(Resource, rdf:type, rdfs:'Class', File)
		),
		fail
	    ;	true
	    ),
	    rdfe_assert(Resource, rdfs:subClassOf, Super, File)
	;   rdfe_assert(Resource, rdf:type, Super, File)
	),
	rdfe_assert(Resource, rdfs:label, literal(Label), File).

local_uri_from_label(_, Label, Local) :-
	new(S, string('%s', Label)),
	send(S, translate, ' ', '_'),
	get(S, value, Local),
	free(S).

:- pce_end_class(rdf_create_dialog).


:- pce_begin_class(rdf_ns_menu, menu,
		   "Prompt for namespace").

initialise(M, Default:[name], Msg:[code]*) :->
	"Create from Default (short id)"::
	send_super(M, initialise, namespace, cycle, Msg),
	findall(NS, rdf_db:ns(NS, _), List0),
	sort(List0, List),
	(   member(NS, List),
	    send(M, append, menu_item(NS, @default, NS)),
	    fail
	;   true
	),
	(   Default \== @default,
	    get(M, member, Default, MI)
	->  send(M, selection, MI)
	;   true
	),
	send(M, show_label, @off).

:- pce_end_class(rdf_ns_menu).

:- pce_begin_class(rdf_file_menu, menu,
		   "Prompt for file").

initialise(M, Default:[name], Msg:[code]*) :->
	send_super(M, initialise, file, cycle, Msg),
	findall(File, rdf_source(File), List0),
	sort(List0, List),
	(   member(File, List),
	    file_base_name(File, Base),
	    send(M, append, menu_item(File, @default, Base)),
	    fail
	;   true
	),
	(   Default \== @default
	->  (   get(M, member, Default, MI)
	    ->	true
	    ;	send(M, append, new(MI, menu_item(Default, @default)))
	    ),
	    send(M, selection, MI)
	;   true
	).

:- pce_end_class(rdf_file_menu).

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

