/*  File:    rdfs_new_item.pl
    Author:  Jan Wielemaker
    Created: Feb  4 2003
    Purpose: Create new resources
*/

:- module(rdfs_new_item,
	  [
	  ]).
:- use_module(library(pce)).
:- use_module(library(pce_identifier_item)).
:- use_module(semweb(rdf_db)).
:- use_module(semweb(rdfs)).
:- use_module(semweb(rdf_edit)).


		 /*******************************
		 *	       DIALOG		*
		 *******************************/

:- pce_begin_class(rdfs_new_dialog, dialog,
		   "Prompt for creating a new intance").

initialise(D, Class:class=name) :->
	rdfs_label(Class, Label),
	send(D, send_super, initialise,
	     string('New instance for %s', Label)),
	send(D, append, new(I, rdfs_new_item(Class))),
	send(I, show_label),
	send(I, show_resource),
	send(I, show_namespace),
	send(D, append, button(create, message(D, create_individual))),
	send(D, append, button(cancel)),
	send(D, default_button, create).

create_individual(D) :->
	"Create the individual and return its resource"::
	get(D, member, new_individual, NI),
	get(NI, create_individual, Resource),
	send(D, return, Resource).

cancel(D) :->
	send(D, destroy).

:- pce_end_class(rdfs_new_dialog).


		 /*******************************
		 *	   PROMPT VALUES	*
		 *******************************/


:- pce_begin_class(rdfs_new_item, dialog_group,
		   "Prompt for a new resource").

variable(rdf_class, name,  get, "OWL description for new instance").
variable(namespace, name*, get, "Namespace for new object").

initialise(NI, Class:name) :->
	"Create new instance of OWL description"::
	send_super(NI, initialise, new_individual, box),
	send(NI, slot, rdf_class, Class).
	
:- pce_group(identity).

show_namespace(NI) :->
	"Add item to select namespace"::
	(   get(NI, namespace, Default),
	    atom(Default)
	->  true
	;   get(NI, rdf_class, Class),
	    rdf_global_id(NS:_Local, Class),
	    rdf_db:ns(NS, Default)
	),
	send(NI, append, rdf_namespace_item(@default, Default)).
	
show_resource(NI) :->
	"Add item to enter the resource"::
	send(NI, append, new(I, identifier_item(resource))),
	send(I, blank, '_').

show_label(NI) :->
	"Add item for label"::
	send(NI, append, rdf_new_label_item(label)).

selection(NI, Name:name, Value:unchecked) :<-
	"Get selection for named item"::
	get(NI, member, Name, Item),
	get(Item, selection, Value).

create_individual(NI, Resource:name) :<-
	"Create the individual and return its resource"::
	get(NI, selection, label, Label),
	get(NI, selection, resource, LocalResource),
	get(NI, selection, namespace, NS),
	atom_concat(NS, LocalResource, Resource),
	get(NI, rdf_class, Class),
	rdfe_transaction((rdfe_assert(Resource, rdf:type, Class),
			  rdfe_assert(Resource, rdfs:label, literal(Label)))).

:- pce_end_class(rdfs_new_item).


		 /*******************************
		 *	  NAMESPACE ITEM	*
		 *******************************/

:- pce_begin_class(rdf_namespace_item, text_item,
		   "Select or define a namespace").

initialise(I,
	   Name:name=[name],
	   Default:default=[name],
	   Message:message=[code]*) :->
	default(Name, namespace, TheName),
	send_super(I, initialise, TheName, Default, Message),
	send(I, length, 40),
	new(ValueSet, chain),
	forall(rdf_db:ns(_, NS),
	       send(ValueSet, append, NS)),
	send(ValueSet, sort),
	send(I, value_set, ValueSet).

:- pce_end_class(rdf_namespace_item).

:- pce_begin_class(rdf_new_label_item, identifier_item,
		   "Enter a new label and synchronise resource").

typed(I, Event:'event|event_id') :->
	send_super(I, typed, Event),
	(   get(I?device, member, resource, ResourceItem)
	->  get(I?value_text, string, Current),
	    get(@pce, convert, Current, string, CS),
	    get(CS, strip, Stripped),
	    send(Stripped, translate, ' ', '_'),
	    send(ResourceItem, string, Stripped)
	;   true
	).

:- pce_end_class(rdf_new_label_item).
