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


:- module(rdf_table, []).
:- use_module(library(pce)).
:- use_module(library(tabular)).
:- use_module(semweb(rdf_db)).
:- use_module(semweb(rdfs)).
:- use_module(owl).
:- use_module(semweb(rdf_edit)).
:- use_module(library(rdf_template)).
:- use_module(rdf_rules).
:- use_module(rdf_cache).
:- use_module(rdf_util).

:- pce_autoload(rdfs_resource_item,	 library(rdfs_resource_item)).
:- pce_autoload(rdf_literal_item,	 library(rdf_literal_item)).

	  
		 /*******************************
		 *	      TABLE		*
		 *******************************/

:- pce_begin_class(rdf_tabular, tabular,
		   "Display table with RDF information").
:- use_class_template(rdf_container).

variable(editable, bool := @off,    get, "Can we modify the table?").
variable(cache,	   int*,	    get, "Cached result").

initialise(AL) :->
	send_super(AL, initialise),
	send(AL, layout_manager, new(T, rdf_property_manager)),
	send(T, rules, all),
	send(T, cell_spacing, -1),
	listen(AL, rdf_transaction(TID), send(AL, update_transaction, TID)).

unlink(AL) :->
	send(AL, detach_cache),
	send_super(AL, unlink).

clear(AL) :->
	"Delete all rows"::
	send(AL, delete_rows).

active(AL, Active:bool) :->
	"(De-)activate tab if present"::
	(   get(AL, container, tab, Tab)
	->  send(Tab, active, Active)
	;   send_super(AL, active, Active)
	).

:- pce_group(update).

detach_cache(AL) :->
	"Detach from the caching system"::
	(   get(AL, cache, Cache), Cache \== @nil
	->  rdf_cache_detach(Cache, AL),
	    send(AL, slot, cache, @nil)
	;   true
	).

update(_AL, _Cache:[int]) :->
	"Called after update of the cache"::
	true.

update_label(AL) :->
	"Update all labels"::
	get(AL, layout_manager, Table),
	send(Table?rows, for_all,
	     if(@arg1 \== @nil,
		message(@arg1, for_all,
			if(message(@arg1, has_send_method, update_label),
			   message(@arg1, update_label))))).

update_transaction(AL, TID:int) :->
	"Update after a transaction"::
	rdfe_transaction_member(TID, file(_)),
	send(AL, update_label).

:- pce_group(edit).


property_on_row(AL, Row:int, PropertyItem:graphical) :<-
	"Find property visualiser at Row"::
	get(AL, layout_manager, Table),
	get(Table, cell, 1, Row, Cell),
	get(Cell, image, Gr),
	(   get(Gr, class_name, graphical)
	->  Row2 is Row - 1,
	    get(AL, property_on_row, Row2, PropertyItem)
	;   PropertyItem = Gr
	).

%	->prompt_value
%	
%	Prompt for a value  for  a   property.  This  deduces  the value
%	restrictions from Property on Subject. On completion it executes
%	Msg using the textual value and one of {literal,resource}.

prompt_value(AL,
	     Msg:message=code,
	     Subject:subject=name,
	     Property:predicate=name,
	     Default:object=[prolog],
	     Label:label=[name],
	     For:for=[graphical]) :->
	"Prompt for a (new) value"::
	map_default(Default, TheDefault),
	(   Label == @default
	->  rdfs_ns_label(Subject, SubjectLabel),
	    rdfs_ns_label(Property, PropertyLabel),
	    new(Lbl, string('Modify %s of %s', PropertyLabel, SubjectLabel))
	;   Lbl = Label
	),
	new(D, dialog(Lbl)),
	new(_, partof_hyper(AL, D, prompter, item)),
	property_domain(Subject, Property, Domain),
	(   Domain = all_values_from(LiteralClass),
	    rdfs_subclass_of(LiteralClass, rdfs:'Literal')
	->  new(Item, rdf_literal_item(Property, LiteralClass, TheDefault)),
	    Type = literal
	;   new(Item, rdfs_resource_item(Property, TheDefault, @nil, Domain)),
	    Type = resource
	),
	send(D, append, Item),
	send(D, append, button(ok,
			       and(message(Msg, forward,
					   Item?selection, Type),
				   message(D, destroy)))),
	send(D, append, button(cancel, message(D, destroy))),
	send(D, default_button, ok),
	    
	send(D, transient_for, AL?frame),
	send(D, modal, transient),
	(   For == @default
	->  send(D, open_centered, AL?frame?area?center)
	;   get(For, display_position, point(X, Y)),
	    send(D, open, point(X, Y+20))
	).

map_default('__not_filled', @default) :- !.
map_default(X, X).


:- pce_end_class(rdf_tabular).


:- pce_begin_class(rdf_resource_cell, table_cell).

initialise(C, R:prolog) :->
	get(C, create_label, R, Label),
	send_super(C, initialise, Label).

create_label(C, R:prolog, Label:graphical) :<-
	call_rules(C, label(R, Label)).

update_label(C) :->
	"Check for possibly changed label classification"::
	get(C, resource, Resource),
	call_rules(C, label_class(Resource, LabelClass)),
	(   get(C?image, class_name, LabelClass)
	->  true
	;   call_rules(C, label(Resource, NewLabel)),
	    send(C, image, NewLabel)
	).

resource(C, Value:prolog) :<-
	"Represented value"::
	get(C, image, Image),
	get(Image, resource, Value).

triple(Cell, Triple:prolog) :<-
	get(Cell, image, Image),
	get(Image, triple, Triple).

:- pce_end_class.

:- pce_begin_class(rdf_subject_cell, rdf_resource_cell).
:- pce_end_class.

:- pce_begin_class(rdf_predicate_cell, rdf_resource_cell).

:- pce_group(edit).

add_value(Cell) :->
	"Add new value from menu"::
	get(Cell, triple, rdf(S,P,_)),
	get(Cell, image, Image),
	get(Image, device, Tabular),
	send(Tabular, prompt_value,
	     message(Cell, do_add_value, @arg1, @arg2),
	     S, P, @default, @default, Image).

do_add_value(Cell, Value:name, Type:{resource,literal}) :->
	(   Type == literal
	->  Object = literal(Value)
	;   Object = Value
	),
	get(Cell, triple, rdf(S,P,_)),
	rdf_add_object(S,P,Object).

delete_all_values(Cell) :->
	"Delete all values for this property"::
	get(Cell, triple, rdf(S,P,_)),
	rdfe_transaction(rdfe_retractall(S,P,_),
			 delete_all_values).

view_triples(N) :->
	"View triples on this relation"::
	get(N, resource, P),
	rdf_cache(rdf(S,P,O), rdf(S,P,O), Cache),
	call_rules(N, show_triple_cache(Cache)).

:- pce_end_class.

:- pce_begin_class(rdf_inferred_object_cell, rdf_resource_cell).

variable(predicate, name*, get, "Related predicate").

initialise(C, R:prolog, P:[name]) :->
	send_super(C, initialise, R),
	(   P \== @default
	->  send(C, slot, predicate, P)
	;   true
	).

create_label(C, R:prolog, Label:graphical) :<-
	call_rules(C, label(R, Label)).

:- pce_end_class(rdf_inferred_object_cell).


:- pce_begin_class(rdf_object_cell, rdf_inferred_object_cell).

delete(Cell) :->
	"Delete triple from database"::
	get(Cell, triple, rdf(S,P,O)),
	rdfe_transaction(rdfe_retractall(S,P,O),
			 delete_property).

modify(Cell) :->
	get(Cell, triple, rdf(S,P,O)),
	get(Cell, image, Image),
	get(Image, device, Tabular),
	send(Tabular, prompt_value,
	     message(Cell, set_value, @arg1, @arg2),
	     S, P, O,
	     @default, Image).
	     
set_value(Cell, Value:name, Type:{resource,literal}) :->
	(   Type == literal
	->  Object = literal(Value)
	;   Object = Value
	),
	get(Cell, triple, rdf(S,P,O)),
	rdf_set_object(S,P,O,Object).

type(Cell, Type:{resource,literal}) :->
	"Change between resource and literal"::
	get(Cell, triple, rdf(S,P,O)),
	(   Type == literal
	->  Object = literal('')
	;   Object = '__not_filled'
	),
	property_domain(S,P,Domain),
	(   Object = literal(_)
	->  (   owl_satisfies(Domain, Object)
	    ->  true
	    ;   rdfs_ns_label(P, PN),
		rdfs_ns_label(S, SN),
	        send(Cell, report, warning,
		     'The range of %s on %s does not allow for a literal value',
		     PN, SN),
		fail
	    )
	;   true
	),
	rdfe_transaction(rdfe_update(S,P,O, object(Object)),
			 modify_property_type).

:- pce_end_class(rdf_object_cell).

:- pce_begin_class(rdf_domain_cell, rdf_object_cell,
		   "Represent a slot-domain").

variable(property, name*, get, "Represented property").

initialise(C, Property:name) :->
	send(C, slot, property, Property),
	(   rdf_has(Property, rdfs:domain, Domain)
	->  send_super(C, initialise, Domain)
	;   rdf_equal(rdfs:'Resource', Domain),
	    send_super(C, initialise, Domain)
	).

triple(C, Triple:prolog) :<-
	"Represented triple"::
	get(C, property, Subject),
	get(C, resource, Object),
	(   rdf_has(Subject, rdfs:domain, Object, Property)
	->  true
	;   rdf_equal(rdfs:domain, Property)
	),
	Triple = rdf(Subject, Property, Object).

:- pce_end_class.


:- pce_begin_class(rdf_range_cell, rdf_object_cell,
		   "Represent a slot-range").

variable(property, name*, get, "Represented property").

initialise(C, Property:name) :->
	send(C, slot, property, Property),
	(   rdf_has(Property, rdfs:range, Range)
	->  send_super(C, initialise, Range)
	;   rdf_equal(rdfs:'Resource', Range),
	    send_super(C, initialise, Range)
	).

triple(C, Triple:prolog) :<-
	"Represented triple"::
	get(C, property, Subject),
	get(C, resource, Object),
	(   rdf_has(Subject, rdfs:range, Object, Property)
	->  true
	;   rdf_equal(rdfs:range, Property)
	),
	Triple = rdf(Subject, Property, Object).

:- pce_end_class.


		 /*******************************
		 *     SIMPLE TRIPLE TABLE	*
		 *******************************/

:- pce_begin_class(rdf_triple_table, rdf_tabular,
		   "Show set of triples").

initialise(T, Triples:[prolog]) :->
	"Create from triples"::
	send_super(T, initialise),
	get(T, layout_manager, Table),
	get(Table, column, 1, @on, C1),
	get(Table, column, 2, @on, C2),
	get(Table, column, 3, @on, C3),
	new(R, rubber),
	send(R, minimum, 100),
	send_list([C1,C2,C3], rubber, R),
	(   Triples \== @default
	->  send(T, triples, Triples)
	;   true
	).


display_title(T) :->
	"Display title row"::
	send(T, append, subject?label_name,
	     bold, halign := center, background := khaki1),
	send(T, append, predicate?label_name,
	     bold, halign := center, background := khaki1),
	send(T, append, object?label_name,
	     bold, halign := center, background := khaki1),
	send(T, next_row).


triples(T, Triples:prolog) :->
	"Show set of triples"::
	send(T, clear),
	send(T, display_title),
	forall(member(rdf(S,P,O), Triples),
	       send(T, triple, S, P, O)).


triple(T, Subject:name, Predicate:name, Object:prolog) :->
	"Append a row with a triple"::
	send(T, append, rdf_subject_cell(Subject)),
	send(T, append, rdf_predicate_cell(Predicate)),
	send(T, append, rdf_object_cell(Object, Predicate)),
	send(T, next_row).

:- pce_end_class(rdf_triple_table).


:- pce_begin_class(rdf_cached_triple_table, rdf_triple_table,
		   "Show table of plain triples from a result-cache").

%	->initialise: Cache:int
%	
%	Create a triple table from a cache identifier.  The cache must
%	return triples of the form rdf(Subject, Predicate, Object).

initialise(T, Cache:[int]) :->
	send_super(T, initialise),
	default(Cache, @nil, TheCache),
	send(T, cache, TheCache).

cache(T, Cache:int*) :->
	"Change the cache"::
	(   get(T, cache, Cache)
	->  true
	;   send(T?window, scroll_to, point(0,0)),
	    send(T, detach_cache),
	    send(T, clear),
	    send(T, slot, cache, Cache),
	    rdf_cache_attach(Cache, T),
	    send(T, update)
	).

value(T, Cache:int*) :->
	send(T, cache, Cache).

%	->update
%	
%	Update after cache change.  TBD: incremental update!

update(T, _Cache:[int]) :->
	"Update"::
	send(T, clear),
	send(T, display_title),
	get(T, cache, Cache),
	(   Cache == @nil
	->  send(T, active, @off)
	;   send(T, active, @on),
	    rdf_cache_result_set(Cache, Triples),
	    functor(Triples, _, Cardinality),
	    (   between(1, Cardinality, I),
		arg(I, Triples, Triple),
		(   Triple = rdf(S,P,O)
		->  send(T, triple, S, P, O)
		;   throw(error(type_error(triple, Triple), _))
		),
		fail
	    ;   true
	    )
	).

:- pce_end_class(rdf_cached_triple_table).


		 /*******************************
		 *	THE LAYOUT MANAGER	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Some serious hacking to adjust the  margin of wrapped text-objects. This
needs more thought in more  abstract  XPCE   classes,  but  for now this
appears to do the job reasonably.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(rdf_property_manager, table,
		   "The layout manager rdf_class_sheet").

stretched_column(Table, Col:table_column, W:int) :->
	"Adjust the size of cells holding a wrapped text"::
	get(Col, index, Index),
	send(Col, for_all, message(Table, stretched_cell, @arg1, W, Index)),
	send_super(Table, stretched_column, Col, W).

stretched_cell(T, Cell:table_cell, W:int, ColN:int) :->
	(   get(Cell, image, Graphical),
	    (	send(Graphical, instance_of, rdf_literal_text)
	    ;	send(Graphical, instance_of, rdf_list_label)
	    )				% TBD: how to generalise?
	->  get(Cell, col_span, Span),
	    get(Cell, column, Col0),
	    EndCol is Col0+Span,
	    cell_width(Col0, EndCol, ColN, W, T, 0, TotalW),
	    TextW is TotalW - 15,
	    send(Graphical, margin, TextW, wrap)
	;   get(Cell, image, Graphical),
	    get(Graphical, class, device)
	->  get(Cell, col_span, Span),
	    get(Cell, column, Col0),
	    EndCol is Col0+Span,
	    cell_width(Col0, EndCol, ColN, W, T, 0, TotalW),
	    TextW is TotalW - 15,
	    send(Graphical, format, width, TextW)
	;   true
	).

%	Determine the width of a spanned cell.

cell_width(End, End, _, _, _, W, W) :- !.
cell_width(C, End, N, W, T, W0, Width) :-
	(   C == N
	->  W1 is W0 + W
	;   get(T, column, C, Col),
	    get(Col, width, WC),
	    W1 is W0 + WC
	),
	C2 is C + 1,
	cell_width(C2, End, N, W, T, W1, Width).

:- pce_end_class(rdf_property_manager).

