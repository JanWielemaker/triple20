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
:- use_module(rdf_render).
:- use_module(library(rdf_template)).

:- pce_autoload(rdfs_resource_item,	 library(rdfs_resource_item)).
:- pce_autoload(rdf_literal_item,	 library(rdf_literal_item)).
:- pce_autoload(rdf_object_list_browser, library(rdf_collection_item)).
:- pce_autoload(editable_text,		 library(pce_editable_text)).

	  
		 /*******************************
		 *	      TABLE		*
		 *******************************/

:- pce_begin_class(rdf_tabular, tabular,
		   "Display table with RDF information").
:- use_class_template(rdf_container).

variable(editable,	 bool := @off, get, "Can we modify the table?").


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


append_resource(AL, Range:name) :->
	"Append a general resource"::
	send(AL, append, rdf_resource_text(Range, AL)).

	    
append_value(AL, Subject:name, Predicate:name, Object:prolog) :->
	"Append object-part of the given triple triple"::
	rdf_render_object_with(rdf(Subject, Predicate, Object), AL, Class),
	NewTerm =.. [Class, Subject, Predicate, Object, AL],
	new(ObjGraphical, NewTerm),
	send(AL, append, ObjGraphical).


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



:- pce_begin_class(rdf_resource_text, text,
		   "Visualize a resource as a text object").

variable(resource,  name,  get, "Represented resource").

initialise(T, Resource:name, Container:[object]) :->
	"Create from resource and table"::
	get(Container, node_label, Resource, Label),
	send_super(T, initialise, Label),
	send(T, slot, resource, Resource),
	(   rdfs_individual_of(Resource, rdfs:'Class')
	->  send(T, colour, blue)
	;   send(T, colour, forestgreen)
	),
	send(T, underline, @on).


resource(T, Resource:name) :->
	"Modify the represented resource"::
	(   get(T, resource, Resource)
	->  true
	;   send(T, slot, resource, Resource),
	    get(T, device, Table),
	    get(Table, node_label, Resource, Label),
	    send(T, string, Label)
	).

:- pce_global(@rdf_resource_text_popup,
	      make_resource_text_popup).
:- pce_global(@rdf_resource_text_recogniser,
	      make_resource_text_recogniser).

make_resource_text_popup(P) :-
	new(P, popup),
	send_list(P, append,
		  [ menu_item(hierarchy_location,
			      message(@arg1, show_details, hierarchy),
			      condition := message(@arg1,
						   can_show_details,
						   hierarchy)),
		    menu_item(details,
			      message(@arg1, show_details, table),
			      condition := message(@arg1,
						   can_show_details,
						   table)),
		    menu_item(view_rdf_source,
			      message(@arg1, view_rdf_source))
		  ]).


make_resource_text_recogniser(G) :-
	new(CG, click_gesture(left, '', single,
			      message(@receiver, show_details, hierarchy))),
	new(PG, popup_gesture(@receiver?popup)),
	new(G, handler_group(CG, PG)).

popup(_T, Popup:popup) :<-
	Popup = @rdf_resource_text_popup.

event(T, Ev:event) :->
	(   send_super(T, event, Ev)
	;   send(@rdf_resource_text_recogniser, event, Ev)
	).

:- pce_group(edit).

show_details(T, How:{hierarchy,table}) :->
	"Show details in format"::
	(   get(T, frame, Frame),
	    send(Frame, has_send_method, show_resource)
	->  get(T, resource, Resource),
	    send(Frame, show_resource, Resource, How)
	).

can_show_details(T, _How:{hierarchy,table}) :->
	"Test if we are embedded in a context that can show details"::
	get(T, frame, Frame),
	send(Frame, has_send_method, show_resource).


view_rdf_source(T) :->
	"Open Prolog editor on RDF source"::
	get(T, resource, Id),
	(   rdf_source_location(Id, File:Line)
	->  edit(file(File, line(Line)))
	;   send(T, report, warning, 'Cannot find source for %s', Id)
	).

prompt(T, Subject:name, Property:name, Default:[name], Label:[name]) :->
	"Prompt for a (new) value"::
	get(T, device, AL),
	send(AL, prompt_value,
	     message(T, edited, @arg1, @arg2),
	     Subject,
	     Property,
	     Default,
	     Label,
	     T).

:- pce_end_class(rdf_resource_text).


:- pce_begin_class(rdf_object_text, rdf_resource_text,
		   "Represent the object-part of a triple").

variable(subject,   name*, get, "RDF subject").
variable(predicate, name*, get, "RDF predicate").

initialise(T,
	   Subject:subject=name,
	   Predicate:predicate=name,
	   Value:object=name,
	   Table:container=[object]) :->
	send_super(T, initialise, Value, Table),
	send(T, slot, subject, Subject),
	send(T, slot, predicate, Predicate).

refresh(T) :->
	get(T, subject, Subject),
	get(T, predicate, Predicate),
	rdf(Subject, Predicate, Value),
	(   atom(Value)
	->  send(T, resource, Value),
	    send(T, background, @nil)
	;   send(T, string, '<no resource>'),
	    send(T, background, red)
	).

:- pce_global(@rdf_object_text_popup,
	      make_object_text_popup).

make_object_text_popup(@Ref) :-
	get(@rdf_resource_text_popup, clone, P),
	send(P, name_reference, Ref),
	send_list(P, append,
		  [ gap,
		    menu_item(modify,
			      message(@arg1, modify)),
		    menu_item(delete,
			      message(@arg1, delete))
		  ]).

popup(_T, Popup:popup) :<-
	Popup = @rdf_object_text_popup.

modify(T) :->
	"Modify the value"::
	get(T, subject, Subject),
	get(T, predicate, Predicate),
	get(T, resource, Object),
	send(T, prompt, Subject, Predicate, Object).

edited(T, New:name, _Type:name) :->
	"A new values was set by the user"::
	get(T, subject, Subject),
	get(T, predicate, Property),
	get(T, resource, Old),
	(   Old \== New
	->  rdfe_transaction(rdfe_update(Subject, Property, Old,
					 object(New)))
	;   true
	).

delete(T) :->
	"Delete the value"::
	send(@display, confirm, 'Really delete?'),
	get(T, subject, Subject),
	get(T, predicate, Property),
	get(T, resource, Old),
	rdfe_transaction(rdfe_retractall(Subject, Property, Old)).

:- pce_end_class(rdf_object_text).


:- pce_begin_class(rdf_predicate_text, rdf_resource_text,
		   "Represent a predicate").

variable(subject,   name*, get, "RDF subject").

initialise(T,
	   Subject:subject=name,
	   Predicate:predicate=name,
	   Table:table=rdf_tabular) :->
	send_super(T, initialise, Predicate, Table),
	send(T, slot, subject, Subject).

:- pce_global(@rdf_predicate_text_popup,
	      make_predicate_text_popup).

make_predicate_text_popup(P) :-
	new(P, popup),
	send_list(P, append,
		  [ menu_item(hierarchy_location,
			      message(@arg1, hierarchy_location)),
		    menu_item(details,
			      message(@arg1, details)),
		    menu_item(view_rdf_source,
			      message(@arg1, view_rdf_source)),
		    gap,
		    menu_item(add_value,
			      message(@arg1, add_value),
			      condition := message(@arg1, can_add_value))
		  ]).

popup(_T, Popup:popup) :<-
	Popup = @rdf_predicate_text_popup.

can_add_value(T) :->
	"Test cardinality constraints"::
	(   get(T, cardinality_restriction, cardinality(_Min, Max))
	->  get(T, cardinality, Count),
	    Count < Max
	;   true			% Nothing known
	).

add_value(T) :->
	"Prompt for a new value"::
	get(T, window, Window),
	get(T, subject, Subject),
	get(T, resource, Predicate),
	get(Window, node_label, Subject, SubjectLabel),
	get(Window, node_label, Predicate, PropertyLabel),
	new(Label, string('Extend %s of %s', PropertyLabel, SubjectLabel)),
	send(T, prompt, Subject, Predicate, @default, Label).

edited(T, Value:name, Type:{literal,resource}) :->
	"A new values was set by the user"::
	get(T, subject, Subject),
	get(T, resource, Predicate),
	(   Type == literal
	->  Object = literal(Value)
	;   Object = Value
	),
	rdfe_transaction(rdfe_assert(Subject, Predicate, Object)).

cardinality(T, Count:int) :<-
	"Count facts on this property"::
	get(T, subject, Subject),
	get(T, resource, Predicate),
					% TBD: use rdf_has()?
	findall(V, rdf(Subject, Predicate, V), L),
	length(L, Count).

cardinality_restriction(T, Card:prolog) :<-
	"Get the cardinality constraints"::
	get(T, subject, Subject),
	get(T, resource, Predicate),
	owl_cardinality_on_subject(Subject, Predicate, Card).

:- pce_end_class(rdf_predicate_text).


		 /*******************************
		 *	   LITERAL TEXT		*
		 *******************************/


:- pce_begin_class(rdf_literal_text, editable_text,
		   "Text object for literal values").

variable(subject,   name*, get,	"RDF subject").
variable(predicate, name*, get,	"Represented predicate").
variable(literal,   name*, get, "Represented object (=value)").

initialise(LT,
	   Subject:subject=name,
	   Predicate:predicate=name,
	   Value:object=prolog,
	   _Container:container=[object]) :->
	(   Value = literal(Text)
	->  true
	;   Text = Value
	),
	send_super(LT, initialise, Text),
	send(LT, margin, 400, wrap),
	send(LT, slot, subject, Subject),
	send(LT, slot, predicate, Predicate),
	send(LT, slot, literal, Text).

refresh(LT) :->
	"Update represented text"::
	get(LT, subject, Subject),
	get(LT, predicate, Predicate),
	(   rdf(Subject, Predicate, literal(Text))
	->  send(LT, literal, Text),
	    send(LT, string, Text)
	;   send(LT, literal, @nil),
	    send(LT, string, '')
	).

object(LT, Object:prolog) :<-
	"Get RDF object: literal(Text)"::
	get(LT, literal, Text),
	Object = literal(Text).
	

obtain_focus(T) :->
	"Start editing"::
	(   get(T, show_caret, @on)
	->  send_super(T, obtain_focus)
	;   send_super(T, obtain_focus),
	    send(T, save_parameter, background),
	    send(T, border, 0),
	    send(T, pen, 0),
	    send(T, background, colour(white))
	).

:- pce_global(@rdf_literal_text_popup,
	      make_literal_text_popup).
:- pce_global(@rdf_literal_text_recogniser,
	      new(popup_gesture(@receiver?popup))).

make_literal_text_popup(P) :-
	new(P, popup),
	send_list(P, append,
		  [ menu_item(delete,
			      message(@arg1, delete))
		  ]).


popup(_T, Popup:popup) :<-
	Popup = @rdf_literal_text_popup.

event(T, Ev:event) :->
	(   send_super(T, event, Ev)
	;   send(@rdf_literal_text_recogniser, event, Ev)
	).

delete(T) :->
	"Delete associated triple"::
	get(T, subject, Subject),
	get(T, predicate, Predicate),
	get(T, object, Text),
	rdfe_transaction(rdfe_retractall(Subject, Predicate,
					 literal(Text))).

forward(T) :->
	"Set new value"::
	get(T, subject, Subject),
	get(T, predicate, Predicate),
	get(T, object, Object),
	get(T?string, value, NewText),
	New = literal(NewText),
	(   New \== Object
	->  rdfe_transaction(rdfe_update(Subject, Predicate,
				     Object, object(New)))
	;   true
	).

:- pce_end_class(rdf_literal_text).


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
	send(T, append_value, Subject, Predicate, Object),
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
