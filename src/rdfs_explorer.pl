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
:- use_module(semweb(rdfs)).
:- use_module(semweb(rdf_db)).
:- use_module(rdf_cache).
:- use_module(rdf_base).
:- use_module(owl).
:- use_module(rdf_util).
:- use_module(semweb(rdfs)).
:- use_module(rdf_table).
:- use_module(semweb(rdf_edit)).
:- use_module(pce_history).
:- use_module(library(broadcast)).
:- use_module(rdf_template).
:- use_module(rdf_tools).
:- use_module(library(tabbed_window)).
:- use_module(window).

:- pce_autoload(report_dialog,	       library(pce_report)).
:- pce_autoload(rdf_statistics_dialog, library(rdf_statistics)).
:- pce_autoload(rdf_namespace_window,  rdf_ns).
:- pce_autoload(finder,		       library(find_file)).
:- pce_global(@finder, new(finder)).


resource(rdfs_explorer_icon, image, image('16x16/hierarchy.xpm')).
resource(new_icon,	     image, image('16x16/new.xpm')).
resource(new_multiple,	     image, image('16x16/newmultiple.xpm')).
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
	send(new(TD, tool_dialog(OV)), above,
	     new(P, constrained_scroll_picture)),
	send(P, name, hierarchy_window),
	send(OV, append, TD),
	send(P, width, 250),
	send(new(D, dialog), above, new(rdfs_sheet)),
	send_list([D,TD], hor_stretch, 100),
	send_list([D,TD], hor_shrink, 100),
	send(D, right, P),
	send(D, pen, 0),
	send(new(report_dialog), below, P),
	new(Tree, rdfs_hierarchy(Domain)),
	send(Tree, name, hierarchy),
	send(P, display, Tree, point(5,5)),
	send(Tree, selectable, @nil),	% allow selecting all nodes
	send(Tree, message, message(OV, show_resource, @arg1, table)),
	send(Tree, expand_domain),
	send(OV, fill_tool_dialog),
	send(OV, fill_dialog),
	get(Tree?root, resource, TheRoot),
	send(OV, show_resource, TheRoot, table),
	listen(OV, rdf_undo(_,_), send(OV, refresh)),
	listen(OV, rdf_transaction(_), send(OV, refresh)),
	listen(OV, rdf_reset, send(OV, refresh)),
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
		  [ new(File,  popup(file)),
		    new(View,  popup(view)),
		    new(Tools, popup(tools))
		  ]),

	send_list(File, append,
		  [ menu_item(new_window),
		    gap,
		    menu_item(open_project),
		    menu_item(new_project),
		    gap,
		    menu_item(load_ontology),
		    new(Base, popup(load_base_ontology,
				    message(OV, load_base_ontology, @arg1))),
			
		    menu_item(new_file),
		    gap,
		    new(SaveFile, popup(save_file,
					message(OV, save_file, @arg1))),
		    menu_item(save_all,
			      condition := message(OV, is_modified)),
		    gap,
		    menu_item(name_spaces),
		    menu_item(statistics),
		    gap,
		    menu_item(exit)
		  ]),

	send_list(View, append,
		  [ new(Label, popup(label,
				     message(OV, view_label_as, @arg1)))
		  ]),

	send_list(Tools, append,
		  [ add_missing_labels
		  ]),

	send(Label, show_current, @on),
	send_list(Label, append,
		  [ label_only,
		    namespace_and_label,
		    resource
		  ]),
	send(Label, selection, namespace_and_label),
	send(SaveFile, update_message,
	     message(OV, update_save_popup, SaveFile)),
	send(Base, update_message,
	     message(OV, update_base_popup, Base)),
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
		    rdf_undo_button(undo),
		    rdf_undo_button(redo),
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
	send(Fields, append, resource),
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
	get(F, member, hierarchy_window, P),
	get(P, member, hierarchy, Tree).

:- pce_group(find).

find(F, String:name, How:name, In:[chain]) :->
	"Highlight nodes holding substring"::
	get(F, tree, Tree),
	send(Tree, collapse_domain),
	get(Tree, device, P),
	send(P, scroll_to, point(0,0)),
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
	    send(Tree?window, normalise, Node?image, y)
	;   How == table
	->  get(OV, member, rdfs_sheet, Sheet),
	    send(Sheet, resource, Resource)
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

load_ontology(_OV) :->
	get(@finder, file, open,
	    chain(tuple('RDF-Schema and OWL files', chain(rdfs,owl)),
		  tuple('OWL files', owl),
		  tuple('RDF files', rdf)),
	    FileName),
	rdfe_transaction(rdfe_load(FileName), load_file(FileName),
			 load_ontology).
      
open_project(OV) :->
	"Open an existing journal file"::
	get(@finder, file, open,
	    tuple('RDF Editor journal', rdfj),
	    FileName),
	rdfe_reset,
	rdfe_open_journal(FileName, append),
	send(OV, set_label).

new_project(OV) :->
	"Create a new journal file"::
	get(@finder, file, save,
	    tuple('RDF Editor journal', rdfj),
	    FileName),
	rdfe_reset,
	rdfe_open_journal(FileName, write),
	send(OV, set_label).

new_file(_OV) :->
	"Add a new file to the project"::
	get(@finder, file, save,
	    chain(tuple('RDF files', rdf),
		  tuple('RDF-Schema files', rdfs),
		  tuple('OWL files', owl)),
	    FileName),
	absolute_file_name(FileName, AbsName),
	rdf_save(FileName, AbsName),
	rdfe_transaction(rdfe_load(FileName), load_file(FileName),
			 new_file).

update_save_popup(_OV, Popup:popup) :->
	"Update menu with all (modified) sources"::
	send(Popup, clear),
	setof(F, rdf_source(F), Files),
	(   member(F, Files),
	    send(Popup, append, new(ME, menu_item(F, @default, F))),
	    (	rdfe_is_modified(F)
	    ->	true
	    ;   send(ME, active, @off)
	    ),
	    fail
	;   true
	).
	
save_file(_OV, File:name) :->
	"Save (export) given file"::
	rdf_save(File, File),
	rdfe_clear_modified(File).

save_all(_OV) :->
	"Save all modified files"::
	(   rdfe_is_modified(File),
	    send(@display, confirm, 'Save %s?', File),
	    rdf_save(File, File),
	    fail
	;   true
	).

is_modified(_OV) :->
	"True if there are modified files"::
	rdfe_is_modified(_), !.

has_project(_OV) :->
	"Test whether a project is defined"::
	rdfe_current_journal(_).

update_base_popup(_OV, Popup:popup) :->
	"Update menu with available base ontologies"::
	send(Popup, clear),
	(   current_base_ontology(Base),
	    send(Popup, append, Base),
	    fail
	;   true
	).

load_base_ontology(_OV, Base:name) :->
	"Load a registered base"::
	rdfe_transaction(load_base_ontology(Base)).

statistics(OV) :->
	"Show elementary statistics"::
	new(D, rdf_statistics_dialog),
	send(D, transient_for, OV),
	send(D, modal, transient),
	send(D, open_centered, OV?area?center).

name_spaces(OV) :->
	new(D, rdf_namespace_window),
	send(D, transient_for, OV),
	send(D, modal, transient),
	send(D, open_centered, OV?area?center).

:- pce_group(preferences).

view_label_as(OV, As:name) :->
	"Determine how a resource is visualised"::
	retractall(rdf_label_rules:view_label_as(_)),
	assert(rdf_label_rules:view_label_as(As)),
	send(@resource_texts, for_all,
	     message(@arg2, for_all,
		     message(@arg1, update))),
	(   get(OV, menu, view, View),
	    get(View, member, label, PopupItem),
	    get(PopupItem, popup, Popup)
	->  send(Popup, selection, As)
	;   true
	).

:- pce_group(tools).

add_missing_labels(OV) :->
	"Add labels to objects that have no label"::
	rdf_equal(rdfs:label, Property),
	rdf_statistics(triples(T0)),		% dubious (threading)
	rdf_add_missing_labels(Property),
	rdf_statistics(triples(T1)),
	TDiff is T1 - T0,
	send(OV, report, status, 'Added %s triples', TDiff).

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


:- pce_begin_class(rdf_undo_button, tool_button,
		   "Button for undo/redo").

initialise(B, Action:{undo,redo}) :->
	send_super(B, initialise,
		   Action,		% action
		   image(resource(Action)),
		   Action,		% tooltip
		   message(B, verify)).
						

verify(B) :->
	"Verify action is available"::
	get(B, name, Action),
	(   Action == undo
	->  rdfe_can_undo(TID)
	;   rdfe_can_redo(TID)
	).

help_message(B, _Which:{tag,summary}, _Ev:[event], Tooltip:char_array) :<-
	"Report on action that will be un/re-done"::
	get(B, name, Action),
	(   (   Action == undo
	    ->  rdfe_can_undo(TID)
	    ;   rdfe_can_redo(TID)
	    ),
	    t_name(TID, Name)
	->  new(Tooltip, string('%s %s', Action?label_name, Name))
	;   get(Action, label_name, Tooltip)
	).

%	t_name(+TID, -Atom)
%	
%	Describe a transaction using the rdfe_transaction_name/2 data.  This
%	must be generalised and centralised as it can also be used for general
%	version management on the journal.

t_name(TID, Atom) :-
	rdfe_transaction_name(TID, Name), !,
	term_to_atom(Name, Atom).
t_name(TID, Atom) :-
	rdfe_transaction_name([TID|_], Name), !,
	term_to_atom(Name, Atom).

:- pce_end_class(rdf_undo_button).




		 /*******************************
		 *	     RDF VIEW		*
		 *******************************/

:- pce_begin_class(rdfs_sheet, tabbed_window,
		   "Visualise a resource as instance or class").
:- use_class_template(rdf_arm).

variable(show_namespace, bool := @on, get, "Show namespaces").
variable(history,	 history,     get, "Location history").

initialise(OS) :->
	send_super(OS, initialise),
	send(OS, slot, history, history(message(OS, resource, @arg1))),
	send(OS, size, size(500,350)),
	send(OS, hor_stretch, 100),
	send(OS, ver_stretch, 100),
	send(OS, ver_shrink, 100),
	send_list(OS, append,
		  [ table_window(class,    new(rdfs_class_sheet)),
		    table_window(instance, new(rdfs_instance_sheet))
		  ]).

sheet(OS, Name:{class,instance}, Table:rdf_tabular) :<-
	"Get named table"::
	get(OS, member, Name, Window),
	get(Window?graphicals, head, Table).

resource(OS, Resource:name) :->
	"Show the indicated object"::
	get(OS, sheet, instance, InstanceSheet),
	get(OS, sheet, class, ClassSheet),
	send(OS?members, for_all,
	     message(@arg1, scroll_to, point(0,0))),
	send(InstanceSheet, resource, Resource),
	(   rdfs_individual_of(Resource, rdfs:'Class')
	->  send(ClassSheet, resource, Resource),
	    send(OS, on_top, class)
	;   send(ClassSheet, resource, @nil),
	    send(OS, on_top, instance)
	),
	send(OS?history, location, Resource).

button(OS, Dir:{forward,backward}, B:tool_button) :<-
	"Get button for history navigation"::
	get(OS?history, button, Dir, B).

:- pce_end_class(rdfs_sheet).


:- pce_begin_class(table_window, dialog,
		   "Window for displaying a single tabular").
:- use_class_template(rdf_arm).

initialise(TT, Name:name, Table:tabular) :->
	send_super(TT, initialise, Name),
	send(TT, pen, 0),
	send(TT, scrollbars, vertical),
	send(TT, name, Name),
	send(TT, display, Table).	% do not use automatic layout

resize(TT) :->
	get(TT?graphicals, head, Table),
	get(TT?size, width, W),
	TW is max(0, W-2),		% table-width excludes the border
	send(Table, table_width, TW).
	
scroll_vertical(TW,
		Direction:{forwards,backwards,goto},
		Unit:{page,file,line},
		Amount:int) :->
	"Prevent scrolling too far"::
	get(TW, visible, VA),
	get(TW, bounding_box, BB),
	(   send(VA, inside, BB)
	->  true
	;   Direction == backwards,
	    get(VA, y, Y),
	    Y < 1
	->  true
	;   Direction == forwards,
	    get(BB, bottom_side, BBBottom),
	    get(VA, bottom_side, VABottom),
	    VABottom > BBBottom
	->  true
	;   send_super(TW, scroll_vertical, Direction, Unit, Amount),
	    get(TW, visible, area(_, AY, _, _)),
	    (   AY < 0
	    ->  send(TW, scroll_to, point(0,0))
	    ;   true
	    )
	).

:- pce_end_class(table_window).


		 /*******************************
		 *	      CLASSES		*
		 *******************************/

:- pce_begin_class(rdfs_class_sheet, rdf_tabular,
		   "Show attributes of a class").

variable(resource,	name*,		get, "Displayed class").

initialise(AL, Class:[name]) :->
	"Create from ontology and class"::
	send_super(AL, initialise),
	(   Class \== @default
	->  send(AL, resource, Class)
	;   true
	),
	property_cache(Cache),
	rdf_cache_attach(Cache, AL),
	listen(AL, rdf_reset, send(AL, resource, @nil)).

unlink(AL) :->
	property_cache(Cache),
	rdf_cache_detach(Cache, AL),
	unlisten(AL),
	send_super(AL, unlink).

property_cache(Cache) :-
	rdf_cache(X, property(X), Cache),
	rdf_cache_cardinality(Cache, _). % force update

property(property(P,D,R)) :-
	rdfs_individual_of(P, rdf:'Property'),
	rdf_has(P, rdfs:domain, D),
	rdf_has(P, rdfs:range, R).

attach_cache(AL) :->
	"Attach to the caching system"::
	(   get(AL, resource, Resource),
	    Resource \== @nil
	->  rdf_cache(X, class_property(Resource, X), Cache),
	    send(AL, slot, cache, Cache),
	    rdf_cache_attach(Cache, AL),
	    rdf_cache_cardinality(Cache, _Card) 	% force cache to update
	;   true
	).

class_property(Class, P=O) :-
	rdf(Class, P, O).

resource(AL, Resource:name*) :->
	(   get(AL, resource, Resource)
	->  true
	;   send(AL, detach_cache),
	    send(AL, slot, resource, Resource),
	    send(AL, attach_cache),
	    send(AL, update)
	).

update(AL, _Cache:[int]) :->
	"Display properties of Class"::
	send(AL, clear),
	get(AL, resource, Class),
	(   Class == @nil
	->  true
	;   send(AL, display_title, Class),
	    send(AL, append_slots_of, Class)
	).


display_title(AL, Class:name) :->
	"Display the title row"::
	send(AL, append, 'Class', bold, right),
	send(AL, append, rdf_resource_text(Class), colspan := 2),
	send(AL, next_row),
	send(AL, display_comment),
	send(AL, display_type),
	send(AL, append_owl_properties, Class),
	send(AL, append, 'Properties', bold, center,
	     colspan := 3, background := khaki1),
	send(AL, next_row),
	send(AL, append, 'Name',     bold, center),
	send(AL, append, 'Domain',   bold, center),
	send(AL, append, 'Range',    bold, center),
	send(AL, next_row).

display_type(AL) :->
	"Display class(es) I belong to"::
	rdf_equal(rdf:type, Property),
	ignore(send(AL, append_property, Property)).

display_comment(AL) :->
	"Display the comment field"::
	rdf_equal(rdfs:comment, Property),
	ignore(send(AL, append_property, Property)).

append_owl_properties(AL, Class:name) :->
	(   owl_property(P),
	    rdf_has(Class, P, _Set, _Prop)
	->  send(AL, append, 'OWL Description facets', bold, center,
		 colspan := 3, background := khaki1),
	    send(AL, next_row),
	    (   owl_property(P2),
		send(AL, append_property, P2),
		fail
	    ;   true
	    )
	;   true
	).

owl_property(P) :- rdf_equal(rdfs:subClassOf, P).
owl_property(P) :- rdf_equal(owl:oneOf, P).
owl_property(P) :- rdf_equal(owl:intersectionOf, P).
owl_property(P) :- rdf_equal(owl:unionOf, P).
owl_property(P) :- rdf_equal(owl:complementOf, P).


append_continuation_value(AL, V:prolog, Pred:[name]) :->
	"Append value in the 2nd column"::
	send(AL, append, new(graphical)),
	send(AL, append, rdf_object_cell(V, Pred), colspan := 2),
	send(AL, next_row).

append_slots_of(AL, Class:name) :->
	"Append normal properties"::
	findall(Name, rdfs_class_property(Class, Name), Names0),
	list_to_set(Names0, Names),
	forall(member(Name, Names),
	       send(AL, append_slot, Name, Class)).


append_slot(AL, Slot:name, Class:[name]) :->
	"Display append a slot"::
	send(AL, append, rdf_predicate_cell(Slot)),
	(   Class == @default
	->  get(AL, class, TheClass)
	;   TheClass = Class
	),
	(   TheClass == @nil
	->  send(AL, append, '<unknown>', italic),
	    send(AL, append, '-')
	;   send(AL, append, rdf_domain_cell(Slot)),
	    send(AL, append, rdf_range_cell(Slot))
	),
	send(AL, next_row).


:- pce_group(object_property).

append_property(AL, Property:name) :->
	"Append slot and its values"::
	get(AL, resource, R),
	bagof(Value, rdf_has(R, Property, Value, P), [V1|Values]),
	(   send(AL, append, rdf_predicate_cell(P)),
	    send(AL, append, rdf_object_cell(V1, P), colspan := 2),
	    send(AL, next_row),
	    forall(member(V, Values),
		   send(AL, append_continuation_value, V, P)),
	    fail
	;   true
	).


append_continuation_value(AL, V:prolog, Pred:[name]) :->
	"Append value in the 2nd column"::
	send(AL, append, new(graphical)),
	send(AL, append, rdf_object_cell(V, Pred), colspan := 2),
	send(AL, next_row).


%	<-triple_from_part: graphical --> rdf(S,P,O)
%	
%	Compute the triple of which graphical is a part.

triple_from_part(AL, Part:graphical, Triple:prolog) :<-
	"Find triple in which Part participates"::
	get(Part, layout_interface, Cell),
	get(AL, resource, S),
	get(Cell?image, resource, CR),
	(   send(Cell, instance_of, rdf_object_cell)
	->  get(Cell, predicate, P),
	    O = CR
	;   send(Cell, instance_of, rdf_predicate_cell)
	->  P = CR
	;   tbd
	),
	Triple = rdf(S, P, O).

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
	->  send(AL, resource, Instance)
	;   true
	),
	listen(AL, rdf_reset, send(AL, resource, @nil)).

unlink(AL) :->
	unlisten(AL),
	send_super(AL, unlink).

attach_cache(AL) :->
	"Attach to the caching system"::
	(   get(AL, resource, Resource),
	    Resource \== @nil
	->  rdf_cache(P=O, rdf(Resource, P, O), Cache),
	    send(AL, slot, cache, Cache),
	    rdf_cache_attach(Cache, AL),
	    rdf_cache_cardinality(Cache, _Card) 	% force cache to update
	;   true
	).

resource(AL, Resource:name*) :->
	(   get(AL, resource, Resource)
	->  true
	;   send(AL, detach_cache),
	    send(AL, slot, resource, Resource),
	    send(AL, attach_cache),
	    send(AL, update)
	).

update(AL, _Cache:[int]) :->
	send(AL, clear),
	(   get(AL, resource, @nil)
	->  true
	;   send(AL, display_title),
	    send(AL, display_predicates_title),
	    send(AL, append_slots)
	).

display_title(AL) :->
	"Display common information"::
	send(AL, display_resource),
	send(AL, display_label),
	send(AL, display_type),
	send(AL, display_comment).

display_label(AL) :->
	"Display the label (if any)"::
	rdf_equal(rdfs:label, Property),
	ignore(send(AL, append_property, Property)).

display_resource(AL) :->
	get(AL, resource, I),
	send(AL, append, 'Resource', bold, right),
	send(AL, append, I),
	send(AL, next_row).

display_type(AL) :->
	"Display class(es) I belong to"::
	rdf_equal(rdf:type, Property),
	ignore(send(AL, append_property, Property)).

display_comment(AL) :->
	"Display the comment field"::
	rdf_equal(rdfs:comment, Property),
	ignore(send(AL, append_property, Property)).

display_predicates_title(AL) :->
	new(D, device),
	send(D, format, new(F, format(vertical, 1, @on))),
	send(F, adjustment, vector(center)),
	send(D, display, text('Predicates', left, bold)),
	send(D, display, new(BM1, bitmap(resource(new_icon)))),
	send(BM1, recogniser,
	     click_gesture(left, '', single,
			   message(AL, add_predicate, @receiver))),
	send(BM1, help_message, tag, 'Add property'),
	(   has_standard_predicates(AL)
	->  send(D, display, new(BM2, bitmap(resource(new_multiple)))),
	    send(BM2, recogniser,
		 click_gesture(left, '', single,
			       message(AL, add_standard_predicates))),
	    send(BM2, help_message, tag, 'Add standard properties')
	;   true
	),
	send(AL, append, D,
	     halign := center, colspan := 2, background := khaki1),
	send(AL, next_row).

	
append_slots(AL) :->
	get(AL, resource, I),
	(   call_rules(AL, visible_predicate(I, Property)),
	    \+ reserved_instance_slot(Property),
	    findall(V, rdf(I, Property, V), Values),
	    sort_by_label(Values, [V1|RestValues]),
	    send(AL, append, rdf_predicate_cell(Property)),
	    send(AL, append, rdf_object_cell(V1, Property)),
	    send(AL, next_row),
	    forall(member(V, RestValues),
		   send(AL, append_continuation_value, V, Property)),
	    fail
	;   true
	).


reserved_instance_slot(Type) :-
	rdfs_subproperty_of(Type, rdf:type).
reserved_instance_slot(Label) :-
	rdfs_subproperty_of(Label, rdfs:label).
reserved_instance_slot(Label) :-
	rdfs_subproperty_of(Label, rdfs:comment).

append_property(AL, Property:name) :->
	"Append slot and its values"::
	get(AL, resource, R),
	bagof(Value, rdf_has(R, Property, Value, P), [V1|Values]),
	(   send(AL, append, rdf_predicate_cell(P)),
	    send(AL, append, rdf_object_cell(V1, P)),
	    send(AL, next_row),
	    forall(member(V, Values),
		   send(AL, append_continuation_value, V, P)),
	    fail
	;   true
	).


append_continuation_value(AL, V:prolog, Property:[name]) :->
	"Append value in the 2nd column"::
	send(AL, append, new(graphical)),
	send(AL, append, rdf_object_cell(V, Property)),
	send(AL, next_row).

:- pce_group(edit).


%	<-triple_from_part: graphical --> rdf(S,P,O)
%	
%	Compute the triple of which graphical is a part.

triple_from_part(AL, Part:graphical, Triple:prolog) :<-
	"Find triple in which Part participates"::
	get(Part, layout_interface, Cell),
	get(AL, resource, S),
	get(Cell?image, resource, CR),
	(   send(Cell, instance_of, rdf_object_cell)
	->  get(Cell, predicate, P),
	    O = CR
	;   send(Cell, instance_of, rdf_predicate_cell)
	->  P = CR
	;   tbd
	),
	Triple = rdf(S, P, O).


add_predicate(AL, From:graphical) :->
	"Add another attribute"::
	(   setof(Pred, missing_subject_predicate(AL, Pred), Preds0),
	    sort_by_label(Preds0, Preds)
	->  new(D, dialog('New predicate')),
	    length(Preds, NPreds),
	    (	NPreds > 20
	    ->	send(D, append, new(LB, list_browser)),
		send(LB, width, 50),
		send(LB, select_message, message(D, return, @arg1?key)),
		(   member(Pred, Preds),
		    rdfs_ns_label(Pred, Label),
		    send(LB, append, dict_item(Pred, Label)),
		    fail
		;   true
		)
	    ;	send(D, append, new(M, menu(predicate, choice,
					    message(D, return, @arg1)))),
		send(M, layout, vertical),
		send(M, multiple_selection, @on),
		(   member(Pred, Preds),
		    rdfs_ns_label(Pred, Label),
		    send(M, append, menu_item(Pred, @default, Label)),
		    fail
		;   true
		)
	    ),
	    send(D, append, button(cancel, message(D, return, @nil))),
	    get(From, display_position, point(X, Y)),
	    send(D, transient_for, AL?frame),
	    send(D, modal, transient),
	    get(D, confirm, point(X, Y+20), Predicate),
	    send(D, destroy),
	    Predicate \== @nil,

	    get(AL, resource, Subject),
	    property_type(Subject, Predicate, Type),
	    (	Type == resource
	    ->	Object = '__not_filled'
	    ;	Object = literal('')
	    ),
	    rdfe_transaction(rdfe_assert(Subject, Predicate, Object),
			     new_property)
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
	rdfe_transaction(rdfe_assert(Subject, Predicate, Object),
			 add_property).

%	missing_subject_predicate(+Sheet, -Property)
%	
%	Enumerate the properties that are defined for the subject, but
%	do not yet have a value.  Note that this should be made more
%	subtle if we include OWL (probably calling rules).

missing_subject_predicate(AL, Property) :-
	get(AL, resource, Subject),
	rdf_has(Subject, rdf:type, Class),
	rdfs_individual_of(Property, rdf:'Property'),
	rdf_has(Property, rdfs:domain, Domain),
	rdfs_subclass_of(Class, Domain),
	\+ rdf(Subject, Property, _).

add_standard_predicates(AL) :->
	"Add missing predicates that should normally be present"::
	rdfe_transaction(add_standard_predicates(AL),
			 add_standard_predicates).

add_standard_predicates(AL) :-
	get(AL, resource, Subject),
	(   call_rules(AL, standard_predicate(Subject, Predicate)),
	    \+ rdf(Subject, Predicate, _),
	    property_type(Subject, Predicate, Type),
	    (	Type == resource
	    ->	Object = '__not_filled'
	    ;	Object = literal('')
	    ),
	    rdfe_assert(Subject, Predicate, Object),
	    fail
	;   true
	).

has_standard_predicates(AL) :-
	get(AL, resource, Subject),
	call_rules(AL, standard_predicate(Subject, Predicate)),
	\+ rdf(Subject, Predicate, _), !.

:- pce_end_class(rdfs_instance_sheet).

