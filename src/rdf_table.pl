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

:- pce_autoload(rdfs_resource_item,	 library(rdfs_resource_item)).
:- pce_autoload(rdf_literal_item,	 library(rdf_literal_item)).
:- pce_autoload(rdf_object_list_browser, library(rdf_collection_item)).
:- pce_autoload(ulan_timestamp_object_item, ulan).

	  
		 /*******************************
		 *	      TABLE		*
		 *******************************/

:- pce_begin_class(rdf_tabular, tabular,
		   "Display table with RDF information").
:- use_class_template(rdf_container).

variable(editable, bool := @off,	    get,  "Can we modify the table?").
variable(rules,	   name := rdf_table_rules, both, "Plugin rules").

initialise(AL) :->
	send_super(AL, initialise),
	send(AL, layout_manager, new(T, rdf_property_manager)),
	send(T, rules, all),
	send(T, cell_spacing, -1),
	listen(AL, rdf_transaction(_), send(AL, refresh)).

unlink(AL) :->
	unlisten(AL),
	send_super(AL, unlink).

refresh(_AL) :->
	"Virtual: update visualisation"::
	true.

clear(AL) :->
	"Delete all rows"::
	send(AL, delete_rows).


append_resource(AL, Resource:prolog, ColSpan:colspan=[int]) :->
	"Append a general resource"::
	rdf_label_rules::label(Resource, Label),
	send(AL, append, Label, colspan := ColSpan).

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
	get(AL, window, Window),
	(   Label == @default
	->  get(Window, node_label, Subject, SubjectLabel),
	    get(Window, node_label, Property, PropertyLabel),
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

%	property_domain(+Subject, +Property, -Domain)
%	
%	Determine the domain of this property. Note that if the domain
%	is a class we want the selector to select a class by browsing
%	the class-hierarchy.  There is some issue around meta-classes
%	here.  Maybe we need class(Root, Meta)!

property_domain(Subject, Property, Domain) :-
	findall(R, property_restriction(Subject, Property, R), List),
	sort(List, Set),
	(   Set = [Domain]
	->  true
	;   Domain = intersection_of(Set)
	).

property_restriction(_, Property, R) :-
	rdf_has(Property, rdfs:range, Range),
	adjust_restriction(all_values_from(Range), R).
property_restriction(Subject, Property, R) :-
	rdf_has(Subject, rdf:type, Class),
	owl_restriction_on(Class, restriction(Property, R0)),
	adjust_restriction(R0, R).
	
adjust_restriction(cardinality(_,_), _) :- !,
	fail.
adjust_restriction(all_values_from(Class), class(Root)) :-
	rdfs_subclass_of(Class, rdfs:'Class'), !,
	rdf_equal(Root, rdfs:'Resource').
adjust_restriction(R, R).

:- pce_end_class(rdf_tabular).


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
	send(T, append_resource, Subject),
	send(T, append_resource, Predicate),
	send(T, append_resource, Object),
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
	    send(Graphical, instance_of, text),
	    get(Graphical, wrap, wrap)
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
