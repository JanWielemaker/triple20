/*  File:    rdf_table.pl
    Author:  Jan Wielemaker
    Created: Feb  1 2003
    Purpose: Display a table of RDF triples
*/

:- module(rdf_table, []).
:- use_module(library(pce)).
:- use_module(library(tabular)).
:- use_module(semweb(rdf_db)).
:- use_module(semweb(rdfs)).
:- use_module(owl).
:- use_module(semweb(rdf_edit)).
:- use_module(library(rdf_template)).
:- use_module(particle).
:- use_module(rdf_cache).
:- use_module(rdf_util).

:- pce_autoload(rdfs_resource_item,	 library(rdfs_resource_item)).
:- pce_autoload(rdf_literal_item,	 library(rdf_literal_item)).
:- pce_autoload(rdf_object_list_browser, library(rdf_collection_item)).

	  
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
	send(T, cell_spacing, -1).

unlink(AL) :->
	send(AL, detach_cache),
	send_super(AL, unlink).

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

clear(AL) :->
	"Delete all rows"::
	send(AL, delete_rows).

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
	->  new(Item, rdf_literal_item(Property, LiteralClass, Default)),
	    Type = literal
	;   new(Item, rdfs_resource_item(Property, Default, @nil, Domain)),
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

:- pce_end_class(rdf_tabular).


:- pce_begin_class(rdf_resource_cell, table_cell).

initialise(C, R:prolog) :->
	call_rules(C, label(R, Label)),
	send_super(C, initialise, Label).

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
:- pce_end_class.

:- pce_begin_class(rdf_object_cell, rdf_resource_cell).

variable(predicate, name*, get, "Related predicate").

initialise(C, R:prolog, P:[name]) :->
	send_super(C, initialise, R),
	(   P \== @default
	->  send(C, slot, predicate, P)
	;   true
	).

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
	(   Type == literal
	->  Object = literal('')
	;   Object = '__not_filled'
	),
	get(Cell, triple, rdf(S,P,O)),
	rdfe_transaction(rdfe_update(S,P,O, object(Object)),
			 modify_property_type).

:- pce_end_class.

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
	(   Triples \== @default
	->  send(T, triples, Triples)
	;   true
	).


display_title(T) :->
	"Display title row"::
	send(T, append, 'Subject',
	     bold, halign := center, background := khaki1),
	send(T, append, 'Predicate',
	     bold, halign := center, background := khaki1),
	send(T, append, 'Object',
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
