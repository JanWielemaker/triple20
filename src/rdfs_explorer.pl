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


:- module(rdfs_explorer,
	  [
	  ]).
:- use_module(library(pce)).
:- use_module(library(toolbar)).
:- use_module(library(hyper)).
:- use_module(rdfs_hierarchy).
:- use_module(library(persistent_frame)).
:- use_module(rdfs).
:- use_module(rdf_db).
:- use_module(owl).
:- use_module(rdf_table).
:- use_module(rdf_edit).
:- use_module(pce_history).
:- use_module(library(broadcast)).

:- pce_autoload(report_dialog,	       library(pce_report)).
:- pce_autoload(rdf_statistics_dialog, library(rdf_statistics)).
:- pce_autoload(finder,		       library(find_file)).
:- pce_global(@finder, new(finder)).


resource(rdfs_explorer_icon, image, image('16x16/hierarchy.xpm')).
resource(new_icon,	     image, image('16x16/new.xpm')).
resource(undo,		     image, image('16x16/undo.xpm')).
resource(redo,		     image, image('16x16/redo.xpm')).
resource(open,		     image, image('16x16/open.xpm')).

:- pce_begin_class(rdfs_explorer, persistent_frame,
		   "Browse an RDFS database").

variable(given_label,	name*,	 get, "User assigned label").

initialise(OV, Domain0:[prolog], Label:[name]) :->
	"Browse an RDFS ontology from given root"::
	(   Label \== @default
	->  send(OV, slot, given_label, Label)
	;   true
	),
	rdf_global_term(Domain0, Domain),
	send_super(OV, initialise),
	send(OV, icon, resource(rdfs_explorer_icon)),
	send(new(TD, tool_dialog(OV)), above, new(P, picture)),
	send(OV, append, TD),
	send(P, width, 250),
	send(new(D, dialog), above, new(rdfs_sheet)),
	send_list([D,TD], hor_stretch, 100),
	send_list([D,TD], hor_shrink, 100),
	send(D, right, P),
	send(D, pen, 0),
	send(new(report_dialog), below, P),
	send(P, display,
	     new(Tree, rdfs_hierarchy(Domain)),
	     point(5,5)),
	send(Tree, selectable, @nil),	% allow selecting all nodes
	send(Tree, message, message(OV, show_resource, @arg1, table)),
	send(Tree, expand_domain),
	send(OV, fill_tool_dialog),
	send(OV, fill_dialog),
	get(Tree?root, resource, TheRoot),
	send(OV, show_resource, TheRoot, table),
	listen(OV, rdf_undo(_,_), send(OV, refresh)),
	listen(OV, rdf_transaction(_), send(OV, refresh)),
	send(OV, refresh),
	send(OV, set_label).

unlink(OV) :->
	unlisten(OV),
	send_super(OV, unlink).

set_label(OV) :->
	(   get(OV, given_label, Format),
	    Format \== @nil
	->  true
	;   Format = 'RDS Browser -- %s'
	),
	(   rdfe_current_journal(Path)
	->  true
	;   Path = '<no project>'
	),
	send(OV, label, string(Format, Path)).

refresh(OV) :->
	get(OV, member, tool_dialog, D),
	get(D, member, tool_bar, TB),
	send(TB, activate).

fill_tool_dialog(OV) :->
	get(OV, member, tool_dialog, D),
	send(D, gap, size(0,1)),
	send(D, border, size(0,0)),
	send_list(D, append,
		  [ new(File, popup(file)),
		    new(View, popup(view))
		  ]),

	send_list(File, append,
		  [ menu_item(new_window),
		    menu_item(open_project,
			      condition := not(message(OV, has_project))),
		    menu_item(new_project,
			      condition := not(message(OV, has_project))),
		    gap,
		    menu_item(load_ontology),
		    gap,
		    menu_item(statistics),
		    gap,
		    menu_item(exit)
		  ]),

	send(View, multiple_selection, @on),
	send(View, show_current, @on),
	send_list(View, append,
		  [ menu_item(namespaces)
		  ]),
	send(View, selected, namespaces, @on),
	send(OV, append_tool_buttons).

append_tool_buttons(OV) :->
	"Append the buttons for this tool"::
	get(OV, member, tool_dialog, D),
	get(OV, member, rdfs_sheet, Sheet),
	get(Sheet, button, forward, Forward),
	get(Sheet, button, backward, Backward),
	send_list(D, append,
		  [ tool_button(open_file,
				image(resource(open)),
				'Open journal or ontology'),
		    gap,
		    tool_button(undo,
				image(resource(undo)),
				'Undo last operation',
				message(@prolog, rdfe_can_undo)),
		    tool_button(redo,
				image(resource(redo)),
				'Undo last undo',
				message(@prolog, rdfe_can_redo)),
		    gap,
		    Backward,
		    Forward
		  ]).


menu(OV, Name:name, Popup:popup) :<-
	"Get named menu from tool"::
	get(OV, member, tool_dialog, TD),
	get(TD, menu_bar, MB),
	get(MB, member, Name, Popup).


fill_dialog(OV) :->
	get(OV, member, dialog, D),
	send(D, pen, 0),
	send(D, border, size(2, 10)),
	send(D, append, new(Fields, menu(search_in, toggle))),
	send(D, append, new(For, label(for, 'For', bold)), right),
	send(For, alignment, left),
	send(D, append, new(SI, text_item(search))),
	send(SI, show_label, @off),
	send(D, append, new(ST, menu(how, cycle)), right),
	send_list(ST, append, [substring, word, prefix, exact]),
	send(ST, show_label, @off),
	send(D, append,
	     new(Find, button(find,
			      message(OV, find,
				      SI?selection, ST?selection,
				      Fields?selection))),
	     right),
	send(OV, search_field, rdfs:label, @on),
	send(OV, search_field, rdfs:comment),
	send(Fields, layout, horizontal),
	send_list([Fields, For], alignment, left),
	send(Find, default_button, @on),
	send(Find, active, @off),
					% HACK: force stretching
	send(D, resize_message,
	     message(OV, resize_dialog, D, @arg2)).


search_field(OV, Field:prolog, Selected:[bool]) :->
	"Define a field for textual search"::
	get(OV, member, dialog, D),
	get(D, member, search_in, Menu),
	rdf_global_id(Field, Global),
	rdfs_label(Global, Label),
	send(Menu, append,
	     new(MI, menu_item(Global, @default, Label))),
	(   Selected == @on
	->  send(MI, selected, Selected)
	;   true
	).


%	->resize_dialog
%	
%	Properly spread the items of the row holding the text-item,
%	menu and button.  Something XPCE should be able to handle
%	using the declarative layout, but this doesn't work.  Ugly
%	but efficient as long as we can't do better.

resize_dialog(_OV, D:dialog, Size:size) :->
	object(Size, size(W,_H)),
	get(D, border, size(BW,_)),
	get(D, gap, size(GW,_)),
	send(D, layout, Size),
	get(D, member, find, Find),
	get(D, member, how, How),
	get(D, member, search, TI),
	get(Find, width, FW),
	get(How, width, HW),
	FX is W-BW-FW-4,
	HX is FX-GW-HW,
	send(Find, x, FX),
	send(How, x, HX),
	send(TI, right_side, HX-GW).

:- pce_group(parts).

tree(F, Tree:rdfs_hierarchy) :<-
	"Get the tree object"::
	get(F, member, picture, P),
	get(P, member, rdfs_hierarchy, Tree).

:- pce_group(find).

find(F, String:name, How:name, In:[chain]) :->
	"Highlight nodes holding substring"::
	get(F, tree, Tree),
	send(Tree, collapse_domain),
	(   In == @default
	->  rdf_global_id(rdfs:label, Label),
	    Fields = chain(Label)
	;   Fields = In
	),
	send(Tree, find_from, String, How, Fields).


		 /*******************************
		 *	      DETAILS		*
		 *******************************/

show_resource(OV, Resource:name, How:name) :->
	"Show given resource"::
	(   How == hierarchy
	->  get(OV, tree, Tree),
	    get(Tree, add, Resource, Node),
	    send(Tree, compute),
	    send(Tree, selection, Node?image),
	    send(Tree?window, normalise, Node?image)
	;   How == table
	->  get(OV, member, rdfs_sheet, Sheet),
	    send(Sheet, selection, Resource)
	).


		 /*******************************
		 *	      ACTIONS		*
		 *******************************/

:- pce_group(file).

new_window(OV) :->
	"Create a second window"::
	get(OV, tree, Tree),
	get(Tree, domain, Domain),
	(   get(OV, given_label, Label),
	    Label \== @nil
	->  true
	;   Label = @default
	),
	get(OV, class, Class),
	get(Class, instance, Domain, Label, NewWindow),
	get(OV?area, position, point(X, Y)),
	send(NewWindow, open, point(X+10, Y+20)).

exit(OV) :->
	"Menu-action `exit'"::
	send(OV, destroy).

open_file(OV) :->
	"Open journal or ontology"::
	(   rdfe_current_journal(_File)
	->  send(OV, load_ontology)
	;   send(OV, open_project)
	).

load_ontology(OV) :->
	get(@finder, file, open,
	    chain(tuple('RDF-Schema and OWL files', chain(rdfs,owl)),
		  tuple('OWL files', owl),
		  tuple('RDF files', rdf)),
	    FileName),
	rdfe_transaction(rdfe_load(FileName)),
	get(OV, tree, Tree),
	send(Tree, refresh).
      
open_project(OV) :->
	"Open an existing journal file"::
	get(@finder, file, open,
	    tuple('RDF Editor journal', rdfj),
	    FileName),
	rdfe_open_journal(FileName, append),
	send(OV, set_label).

new_project(OV) :->
	"Create a new journal file"::
	get(@finder, file, save,
	    tuple('RDF Editor journal', rdfj),
	    FileName),
	rdfe_open_journal(FileName, write),
	send(OV, set_label).

has_project(_OV) :->
	"Test whether a project is defined"::
	rdfe_current_journal(_).


statistics(OV) :->
	"Show elementary statistics"::
	new(D, rdf_statistics_dialog),
	send(D, transient_for, OV),
	send(D, modal, transient),
	send(D, open_centered, OV?area?center).


:- pce_group(preferences).

namespaces(OV) :->
	"Toggle `show namespaces'"::
	get(OV, tree, Tree),
	get(Tree, show_namespace, Show),
	get(Show, negate, NewShow),
	send(Tree, show_namespace, NewShow),
	(   get(OV, menu, view, Popup),
	    get(Popup, member, namespaces, Item)
	->  send(Item, selected, NewShow)
	;   true
	).

:- pce_group(edit).

undo(OV) :->
	"Undo last operation"::
	(   rdfe_undo
	->  true
	;   send(OV, report, warning, 'No further undo')
	).

redo(OV) :->
	"Redo last operation"::
	(   rdfe_redo
	->  true
	;   send(OV, report, warning, 'No further redo')
	).

:- pce_end_class(rdfs_explorer).


		 /*******************************
		 *	     RDF VIEW		*
		 *******************************/

:- pce_begin_class(rdfs_sheet, dialog,
		   "Visualise a resource as instance or class").

variable(show_namespace, bool := @on, get, "Show namespaces").
variable(history,	 history,     get, "Location history").

initialise(OS) :->
	send_super(OS, initialise),
	send(OS, pen, 0),
	send(OS, slot, history, history(message(OS, selection, @arg1))),
	send(OS, size, size(500,350)),
	send(OS, hor_stretch, 100),
	send(OS, ver_stretch, 100),
	send(OS, ver_shrink, 100),
	send(OS, append, new(TS, tab_stack)),
	send(OS, gap, size(0,0)),
	send(OS, resize_message, message(TS, layout_dialog, @arg2)),
	send_list(TS, append,
		  [ table_tab(class,    new(rdfs_class_sheet)),
		    table_tab(instance, new(rdfs_instance_sheet))
		  ]).

sheet(OS, Name:{class,instance}, Table:rdf_tabular) :<-
	"Get named table"::
	get(OS, member, tab_stack, TS),
	get(TS, member, Name, Tab),
	get(Tab?graphicals, head, Table).

selection(OS, Resource:name) :->
	"Show the indicated object"::
	get(OS, sheet, instance, InstanceSheet),
	get(OS, sheet, class, ClassSheet),
	send(InstanceSheet, resource, Resource),
	get(OS, member, tab_stack, TS),
	(   rdfs_individual_of(Resource, rdfs:'Class')
	->  send(ClassSheet, resource, Resource),
	    send(TS, on_top, class)
	;   send(ClassSheet, resource, @nil),
	    send(TS, on_top, instance)
	),
	send(OS?history, location, Resource).

button(OS, Dir:{forward,backward}, B:tool_button) :<-
	"Get button for history navigation"::
	get(OS?history, button, Dir, B).

:- pce_group(label).

node_label(OS, Id:name, Label:name) :<-
	"Get label to display for Id"::
	(   get(OS, show_namespace, @off)
	->  rdfs_label(Id, Label)
	;   rdfs_ns_label(Id, Label)
	).

:- pce_end_class(rdfs_sheet).


:- pce_begin_class(table_tab, tab,
		   "Tab for displaying a single tabular").

initialise(TT, Name:name, Table:tabular) :->
	send_super(TT, initialise, Name),
	send(TT, border, size(1,0)),
	send(TT, append, Table).

size(TT, Size:size) :->
	get(TT?graphicals, head, Table),
	get(Size, width, W),
	TW is max(0, W-4),
	send(Table, table_width, TW),
	send_super(TT, size, Size).
	
:- pce_end_class(table_tab).


		 /*******************************
		 *	      CLASSES		*
		 *******************************/

:- pce_begin_class(rdfs_class_sheet, rdf_tabular,
		   "Show attributes of a class").

variable(resource,	name*,		get, "Displayed class").

initialise(AL, Class:[name]) :->
	"Create from ontology and class"::
	send_super(AL, initialise),
	(   Class \= @default
	->  send(AL, selection, Class)
	;   true
	).

refresh(AL) :->
	get(AL, resource, Class),
	send(AL, resource, Class).

resource(AL, Class:name*) :->
	"Display properties of Class"::
	send(AL, clear),
	send(AL, slot, resource, Class),
	(   Class == @nil
	->  true
	;   send(AL, display_title, Class),
	    send(AL, append_slots_of, Class)
	).


display_title(AL, Class:name) :->
	"Display the title row"::
	rdf_has(Class, rdf:type, Meta),
	send(AL, append, 'Class', bold, right),
	send(AL, append, rdf_resource_text(Class, AL), colspan := 2),
	send(AL, next_row),
	send(AL, append, 'Meta Class', bold, right),
	rdf_equal(rdf:type, TypeProperty),
	send(AL, append,
	     rdf_object_text(Class, TypeProperty, Meta, AL),
	     colspan := 2),
	send(AL, next_row),
	(   rdf_has(Class, rdfs:comment, literal(Comment))
	->  rdf_equal(rdfs:comment, CommentProperty),
	    send(AL, append, 'Comment', bold, right),
	    send(AL, append,
		 rdf_literal_text(Class, CommentProperty, Comment, AL),
		 colspan := 2),
	    send(AL, next_row)
	;   true
	),
	send(AL, append, 'Properties', bold, center,
	     colspan := 3, background := khaki1),
	send(AL, next_row),
	send(AL, append, 'Name',     bold, center),
	send(AL, append, 'Domain',   bold, center),
	send(AL, append, 'Range',    bold, center),
	send(AL, next_row).

append_slots_of(AL, Class:name) :->
	"Append normal properties"::
	findall(Name, rdfs_class_property(Class, Name), Names0),
	list_to_set(Names0, Names),
	forall(member(Name, Names),
	       send(AL, append_slot, Name, Class)).


append_slot(AL, Slot:name, Class:[name]) :->
	"Display append a slot"::
	send(AL, append_resource, Slot),
	(   Class == @default
	->  get(AL, class, TheClass)
	;   TheClass = Class
	),
	(   TheClass == @nil
	->  send(AL, append, '<unknown>', italic),
	    send(AL, append, '-')
	;   (   rdfs_subproperty_of(DomainProperty, rdfs:domain),
	        rdf(Slot, DomainProperty, Base),
	        Base \== TheClass
	    ->  send(AL, append,
		     rdf_object_text(Slot, DomainProperty, Base, AL))
	    ;	send(AL, append, '<self>', italic)
	    ),
	    (   rdfs_subproperty_of(RangeProperty, rdfs:range),
		rdf(Slot, RangeProperty, Range)
	    ->	send(AL, append,
		     rdf_object_text(Slot, RangeProperty, Range, AL))
	    ;	send(AL, append, '-')
	    )
	),
	send(AL, next_row).

:- pce_end_class(rdfs_class_sheet).


		 /*******************************
		 *	     INSTANCES		*
		 *******************************/

:- pce_begin_class(rdfs_instance_sheet, rdf_tabular,
		   "Show attributes of an instance").

variable(resource,	name*,		get, "Displayed Instance").

initialise(AL, Instance:[name]) :->
	"Create from instance"::
	send_super(AL, initialise),
	(   Instance \== @default
	->  send(AL, selection, Instance)
	;   true
	).

resource(AL, Instance:name) :->
	send(AL, clear),
	send(AL, slot, resource, Instance),
	send(AL, display_title),
	send(AL, display_predicates_title),
	send(AL, append_slots).

refresh(AL) :->
	"Re-read the data from the RDF database"::
	get(AL, resource, Resource),
	send(AL, resource, Resource).

display_title(AL) :->
	"Display common information"::
	send(AL, display_label),
	send(AL, display_resource),
	send(AL, display_type),
	send(AL, display_comment).

display_label(AL) :->
	"Display the label (if any)"::
	get(AL, resource, I),
	(   rdfs_subproperty_of(P, rdfs:label),
	    rdf(I, P, Label)
	->  send(AL, append, 'Label', bold, right),
	    send(AL, append_value, I, P, Label),
	    send(AL, next_row)
	;   true
	).

display_resource(AL) :->
	get(AL, resource, I),
	send(AL, append, 'Resource', bold, right),
	send(AL, append, I),
	send(AL, next_row).

display_type(AL) :->
	"Display class(es) I belong to"::
	get(AL, resource, I),
	setof(Class, rdf_has(I, rdf:type, Class), Classes),
	(   Classes = [C1|T]
	->  send(AL, append, 'Class', bold, right),
	    rdf_equal(rdf:type, TypeProperty),
	    send(AL, append_value, I, TypeProperty, C1),
	    send(AL, next_row),
	    forall(member(C, T),
		   send(AL, append_continuation_value, I, TypeProperty, C))
	;   true
	).

display_comment(AL) :->
	"Display the comment field"::
	get(AL, resource, I),
	(   rdf_has(I, rdfs:comment, literal(Comment))
	->  rdf_equal(rdfs:comment, CommentProperty),
	    send(AL, append, 'Comment', bold, right),
	    send(AL, append,
		 rdf_literal_text(I, CommentProperty, Comment, AL)),
	    send(AL, next_row)
	;   true
	).


display_predicates_title(AL) :->
	new(D, device),
	send(D, format, new(F, format(vertical, 1, @on))),
	send(F, adjustment, vector(center)),
	send(D, display, text('Predicates', left, bold)),
	send(D, display,
	     new(B, button(new, message(AL, add_predicate, @receiver)))),
	send(B, label, image(resource(new_icon))),
	send(B, show_focus_border, @off),
	send(AL, append, D,
	     halign := center, colspan := 2, background := khaki1),
	send(AL, next_row).

	
append_slots(AL) :->
	get(AL, resource, I),
	(   bagof(Value, rdf(I, Property, Value), [V1|Values]),
	    \+ reserved_instance_slot(Property),
	    send(AL, append,
		 rdf_predicate_text(I, Property, AL)),
	    send(AL, append_value, I, Property, V1),
	    send(AL, next_row),
	    forall(member(V, Values),
		   send(AL, append_continuation_value, I, Property, V)),
	    fail
	;   true
	).


reserved_instance_slot(Type) :-
	rdfs_subproperty_of(Type, rdf:type).
reserved_instance_slot(Label) :-
	rdfs_subproperty_of(Label, rdfs:label),
	\+ Label= 'http://www.cogsci.princeton.edu/~wn/schema/wordForm'. %BJW
reserved_instance_slot(Label) :-
	rdfs_subproperty_of(Label, rdfs:comment).


append_predicate(AL, P:name) :->
	"Append property name"::
	rdfs_label(P, Label),
	send(AL, append, Label).

append_continuation_value(AL, Subj:name, Property:name, V:prolog) :->
	"Append value in the 2nd column"::
	send(AL, append, new(graphical)),
	send(AL, append_value, Subj, Property, V),
	send(AL, next_row).

:- pce_group(edit).

add_predicate(AL, From:button) :->
	"Add another attribute"::
	get(AL, window, Window),
	(   setof(Pred, missing_subject_predicate(AL, Pred), Preds)
	->  new(D, dialog('New predicate')),
	    send(D, append, new(M, menu(predicate, choice,
					message(D, return, @arg1)))),
	    send(D, append, button(cancel, message(D, return, @nil))),
	    send(M, layout, vertical),
	    send(M, multiple_selection, @on),
	    (   member(Pred, Preds),
		get(Window, node_label, Pred, Label),
		send(M, append, menu_item(Pred, @default, Label)),
		fail
	    ;   true
	    ),
	    get(From, display_position, point(X, Y)),
	    send(D, transient_for, AL?frame),
	    send(D, modal, transient),
	    get(D, confirm, point(X, Y+20), Predicate),
	    send(D, destroy),
	    Predicate \== @nil,

	    get(AL, resource, Subject),
	    send(AL, prompt_value,
		 message(AL, new_predicate, Predicate, @arg1, @arg2),
		 Subject,
		 Predicate,
		 @default,
		 @default,
		 From)
	;   send(AL, report, warning,
		 'No more properties are defined')
	).

new_predicate(AL, Predicate:name, Value:any, Type:{resource,literal}) :->
	"Assert a new value"::
	get(AL, resource, Subject),
	(   Type == literal
	->  Object = literal(Value)
	;   Object = Value
	),
	rdfe_transaction(rdfe_assert(Subject, Predicate, Object)).

%	missing_subject_predicate(+Sheet, -Property)
%	
%	Enumerate the properties that are defined for the subject, but
%	not displayed in the property-sheet.

missing_subject_predicate(AL, Property) :-
	get(AL, resource, Subject),
	rdf_has(Subject, rdf:type, Class),
	rdfs_individual_of(Property, rdf:'Property'),
	rdf_has(Property, rdfs:domain, Domain),
	rdfs_subclass_of(Class, Domain),
					% do we represent this?
					% TBD: cleanup this mess
	\+ get(AL?graphicals, find,
	       and(message(@arg1, instance_of, rdf_predicate_text),
		   @arg1?resource == Property),
	       _),
	\+ rdf_equal(Property, rdf:type).

:- pce_end_class(rdfs_instance_sheet).

