/*  $Id$

    Developed in the MIA project
    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 2000 University of Amsterdam. All rights reserved.
*/

:- module(rdfs_resource_item, []).
:- use_module(library(pce)).
:- use_module(rdfs_hierarchy).
:- use_module(semweb(rdf_db)).
:- use_module(semweb(rdfs)).
:- use_module(owl).

resource(tree,	   image, image('16x16/hierarchy.xpm')).
resource(search,   image, image('16x16/binocular.xpm')).
resource(cut,	   image, image('16x16/cut.xpm')).
resource(synonym,  image, image('16x16/equal.xpm')).
resource(see_also, image, image('16x16/seealso.xpm')).
resource(identity, image, image('16x16/identity.xpm')).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			Class rdfs_resource_item

Select a resource from the loaded ontologies within a defined domain. This
class may return two types of object:

	* An _instance_ satisfying an OWL _restriction_
	* A _class_ satisfying an OWL _description_

See the predicates owl_restriction/2 and  owl_description/2 describe the
Prolog terms to represent this.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(rdfs_resource_item, dialog_group,
		   "Enter resource from ontology").

variable(domain,	       prolog,	     get,  "Set of root-concepts").
variable(selection,	       name*,	     none, "Currently selected label").
variable(allow_abstract,       bool := @off, both, "Allow selecting abstract class").
variable(value_classification, name*,	     get,  "How is the value classified").
variable(instances,	       bool := @off, both, "Return instances").


initialise(OI, Property:name, Selection:[name]*, Message:[code]*,
	   Domain:[prolog]) :->
	rdfs_label(Property, Label),
	send_super(OI, initialise, Label, box),
	send(OI, border, size(10,5)),
	send(OI, gap, size(0,5)),
	send(OI, append, new(TI, rdfs_text_item(text_item, '', Message))),
	send(TI, show_label, @off),
	send(TI, reference, point(0, TI?height)),
	send(TI, style, combo_box),
	send(OI, append, graphical(0,0,5,0), right),
	send(OI, append_button(search)),
	(   Domain == @default
	->  rdf_equal(rdfs:'Resource', Root),
	    send(OI, slot, domain,
		 union_of([class(Root), all_values_from(Root)]))
	;   send(OI, slot, domain, Domain)
	),
	default(Selection, @nil, SelText),
	send(OI, selection, SelText).


append_button(OI, Name:name) :->
	new(B, button(Name)),
	send(B, label, image(resource(Name))),
	send(B, reference, point(0, B?height)),
	send(OI, append, B, right).

append_delete_button(OI) :->
	"Add a button to allow deleteing this field"::
	send(OI, append_button, cut).

cut(OI) :->
	"Remove me from by device"::
	(   send(OI?device, has_send_method, cut)
	->  send(OI?device, cut, OI)
	;   send(OI, destroy)		% Not really cute
	).


modified(OI, Modified:bool) :<-
	"Did user modify value?"::
	get(OI, member, text_item, TI),
	get(TI, modified, Modified).


search(OI) :->
	"Browse hierarchy from current value"::
	get(OI, member, text_item, TI),
	send(new(B, rdfs_select_browser(OI)), open),
	get(B, tree, Tree),
	send(Tree, open_message,
	     and(message(OI, selected_from_search, @arg1),
		 message(B, destroy))),
	(   get(OI, value_classification, ok)
	->  get(OI, selection, Sel),
	    send(B, locate, Sel)
	;   get(OI, value_classification, ambiguous)
	->  get(OI, domain, Domain),
	    get(TI, value, Typed),
	    send(Tree, collapse_domain),
	    (	named_resource(Typed, Class),
		owl_satisfies(Domain, Class),
		get(Tree, add, Class, Node),
		send(Node, selected, @on),
		fail
	    ;	true
	    )
	;   get(TI, value, Typed),
	    (   Typed == ''
	    ->  send(Tree, expand_domain)
	    ;   send(B, find, Typed)
	    )
	).


selected_from_search(OI, Sel:name) :->	% Sel is indentifier!
	(   get(OI, selection, Sel)
	->  true
	;   get(OI, editable, @on),
	    send(OI, selection, Sel),
	    (	get(OI, device, Dev),
		Dev \== @nil
	    ->	ignore(send(Dev, modified_item, OI, @on))
	    ;	true
	    ),
	    ignore(send(OI, apply, @on))
	).


editable(OI, Val:bool) :->
	get(OI, member, text_item, TI),
	send(TI, editable, Val).
editable(OI, Val:bool) :<-
	get(OI, member, text_item, TI),
	get(TI, editable, Val).

message(OI, Msg:[code]*) :->
	get(OI, member, text_item, TI),
	send(TI, message, Msg).
message(OI, Msg:[code]*) :<-
	get(OI, member, text_item, TI),
	get(TI, message, Msg).

apply(OI, Always:[bool]) :->
	"Execute <-message"::
	(   (   Always == @on
	    ;   get(OI, modified, @on)
	    )
	->  (   get(OI, selection, Value),
	        get(OI, message, Message),
		send(Message, instance_of, code)
	    ->	send(Message, forward_receiver, OI, Value)
	    ;	true
	    )
	).

selection(OI, Term:name*) :->
	"Set the selection"::
	send(OI, slot, selection, Term),
	get(OI, member, text_item, TI),
	(   Term == @nil
	->  send(TI, clear)
	;   rdfs_label(Term, Label),
	    send(TI, selection, Label)
	),
	send(OI, classify_term).


selection(OI, Term:name) :<-
	"Fetch selected term"::
	send(OI, classify_value),
	get(OI, slot, selection, Term),
	Term \== @nil.


clear(OI) :->
	"Remove selection"::
	get(OI, member, text_item, TI),
	send(TI, clear).


empty(OI) :->
	"True of ->clear'ed"::
	get(OI, member, text_item, TI),
	get(TI, value, '').


classify_value(OI, Always:[bool]) :->
	"Colour the item to its current class"::
	get(OI, member, text_item, TI),
	(   Always \== @on,
	    get(TI, modified, @off),
	    get(OI, value_classification, Class),
	    Class \== @nil
	->  true
	;   get(TI, value, Typed),
	    get(OI, domain, Domain),
	    classify_value(OI, Typed, Domain, ClassName, Class),
	    (   Class == ok
	    ->  send(OI, slot, selection, ClassName)
	    ;   send(OI, slot, selection, @nil)
	    ),
	    send(OI, slot, value_classification, Class)
	),
	attributes(Class, Attrs),
	send_list(TI, Attrs).


classify_value(OI, Typed, Domain, Term, Class) :-
	findall(Class, named_resource(Typed, Class), Classes),
	(   Classes \== []
	->  in_domain(Classes, Domain, InDomain),
	    (	InDomain = [Term]
	    ->	(   get(OI, allow_abstract, @on)
		->  Class = ok
		;   (   fail		/* TBD: send(O, abstract, Term) */
		    ->  Class = abstract
		    ;   Class = ok
		    )
		)
	    ;	InDomain = []
	    ->	Class = abstract_domain
	    ;	length(InDomain, Len),
		send(OI, report, status, 'Found "%s" %d times in ontology',
		     Typed, Len),
		Class = ambiguous
	    )
	;   Class = unknown
	).

%	named_resource(+Label, -Class)
%	
%	Return resources that have the indicated label

named_resource(Label, Class) :-
	rdf_has(Class, rdfs:label, literal(exact(Label), _)).


attributes(ok,		    [ value_font(bold),   colour(black) ]).
attributes(abstract,	    [ value_font(bold),   colour(grey40) ]).
attributes(abstract_domain, [ value_font(normal), colour(blue) ]).
attributes(ambiguous,	    [ value_font(normal), colour(green) ]).
attributes(unknown,	    [ value_font(normal), colour(red) ]).

classify_term(OI) :->
	"Classify term in selection"::
	get(OI, member, text_item, TI),
	(   get(OI, slot, selection, Term),
	    Term \== @nil
	->  get(OI, domain, Domain),
	    (	owl_satisfies(Domain, Term)
	    ->	(   get(OI, allow_abstract, @on)
		->  Class = ok
		;   (   fail	% send(O, abstract, Term)
		    ->  Class = abstract
		    ;   Class = ok
		    )
		)
	    ;	Class = unknown
	    ),
	    send(OI, slot, value_classification, Class)
	;   send(OI, slot, value_classification, @nil),
	    Class = unknown
	),
	attributes(Class, Attrs),
	send_list(TI, Attrs).
	    

%	in_domain(+TermList, +Domain, -DomainTerms)
%	
%	Find the subset of TermList that is in the specified domain.

in_domain(List, Domain, List) :-
	rdf_equal(Domain, rdfs:'Resource'), !.
in_domain([], _, []).
in_domain([H|T0], Dom, [H|T]) :-
	owl_satisfies(Dom, H),
	in_domain(T0, Dom, T).
in_domain([_|T0], Dom, T) :-
	in_domain(T0, Dom, T).


:- pce_group(completion).

completions(OI, From:char_array, Labels:chain) :<-
	"Provide completions from prefix"::
	get(OI, display, Display),
	send(Display, busy_cursor),
	get(OI, domain, Domain),
	get(From, value, Prefix),
	new(Labels, chain),
	(   complete_label_in_domain(Prefix, Domain, FullLabel),
	    send(Labels, append, FullLabel),
	    fail
	;   true
	),
	send(Labels, sort, unique := @on),
	send(Display, busy_cursor, @nil).

complete_label_in_domain(Prefix, Domain, Label) :-
	rdf_has(Resource, rdfs:label, literal(prefix(Prefix), Label)),
	owl_satisfies(Domain, Resource).

:- pce_end_class(rdfs_resource_item).

:- pce_begin_class(rdfs_text_item, text_item,
		   "Text item with completion").

class_variable(length, '0..', 30, "# characters").

completions(TI, From:char_array, Labels:chain) :<-
	"Provide completions"::
	get(TI, device, OI),
	get(OI, completions, From, Labels).

apply(TI, Always:[bool]) :->
	send(TI?device, apply, Always).

typed(TI, Ev:event_id) :->
	"Check whether it is a valid item now"::
	get(TI, device, Dev),
	send_super(TI, typed, Ev),
	(   object(Dev)
	->  send(Dev, classify_value, @on)
	;   true
	).

selected_completion(TI, Text:char_array, Apply:[bool]) :->
	"User selected from completion browser"::
	send_super(TI, selected_completion, Text, Apply),
	ignore(send(TI?device, classify_value, @on)).

:- pce_end_class(rdfs_text_item).


		 /*******************************
		 *	   BROWSE/SEARCH	*
		 *******************************/

:- pce_autoload(partof_hyper, library(hyper)).
:- pce_autoload(report_dialog, library(pce_report)).

:- pce_begin_class(rdfs_select_browser, frame,
		   "Browse contology tree for selecting").

initialise(F, Selector:rdfs_resource_item) :->
	"Create for selector"::
	send_super(F, initialise, 'Select term'),
	send(F, append, new(D1, dialog(top))),
	send(D1, name, top),
	send(F, fill_top_dialog),
	send(new(B, browser), right, new(P, picture)),
	send(P, below, D1),
	send(new(V, view(size := size(40,5))), below, P),
	send(new(report_dialog), below, V),
	get(Selector, domain, Dom),
	new(T, rdfs_hierarchy(Dom)),
	send(P, display, T, point(5,5)),
	send(T, message, message(F, details, @arg1)),
	(   get(Selector, allow_abstract, @on)
	->  send(T?selectable, append, abstract)
	;   true
	),
	send(V, font, normal),
	send(V, wrap, word),
	send(V, editable, @off),
	send(V, show_caret, @off),
	send(B, style, synonym,  style(icon := image(resource(synonym)))),
	send(B, style, see_also, style(icon := image(resource(see_also)))),
	send(B, style, identity, style(icon := image(resource(identity)))),
	send(B, open_message, message(F, open_from_browser, @arg1)),
	new(_, partof_hyper(Selector, F, hierarchy, item)).

open(F) :->
	"Open the tree just below the item"::
	get(F, hypered, item, Item),
	get(Item, display_position, point(X,Y)),
	get(Item, height, H),
	get(Item, frame, Parent),
	send(F, transient_for, Parent),
	send(F, modal, transient),
	send_super(F, open, point(X, Y+H+5), normalise := @on).

fill_top_dialog(F) :->
	"Fill the seach/select dialog"::
	get(F, member, top, D),
	send(D, border, size(5,5)),
	send(D, append, new(Fields, menu(search_in, toggle))),
	send_list(Fields, append, [label, synonym, comment]),
	send(Fields, selection, label),
	send(Fields, layout, horizontal),
	send(D, append, new(For, label(for, 'For', bold)), right),
	send_list([Fields, For], alignment, left),
	send(For, alignment, left),
	send(D, append, new(SI, text_item(search))),
	send(SI, show_label, @off),
	send(D, append, new(ST, menu(how, cycle)), right),
	send_list(ST, append, [substring, whole_word, prefix, exact]),
	send(ST, show_label, @off),
	send(D, append,
	     new(Find, button(find,
			      and(message(F, find,
					  SI?selection, ST?selection,
					  Fields?selection),
				  message(@receiver, active, @off)))),
	    right),
	send(Find, default_button, @on),
	send(Find, active, @off),
	send(D, append, button(cancel, message(F, destroy)), right),
	send(D, resize_message,
	     message(F, resize_dialog, D, @arg2)).

%	->resize_dialog
%	
%	Properly spread the items of the row holding the text-item,
%	menu and button.  Something XPCE should be able to handle
%	using the declarative layout, but this doesn't work.  Ugly
%	but efficient as long as we can't do better.

resize_dialog(_F, D:dialog, Size:size) :->
	object(Size, size(W,_H)),
	get(D, border, size(BW,_)),
	get(D, gap, size(GW,_)),
	send(D, layout, Size),
	get(D, member, find, Find),
	get(D, member, how, How),
	get(D, member, search, TI),
	get(D, member, cancel, C),
	right_to_left([C,Find,How,TI], GW, W-BW).

right_to_left([], _, _).
right_to_left([T], _, R) :- !,
	send(T, right_side, R).
right_to_left([H|T], G, R) :-
	get(H, width, W),
	X is R-W,
	send(H, x, X),
	R2 is X - G,
	right_to_left(T, G, R2).

tree(F, Tree:rdfs_tree) :<-
	"Get the tree object"::
	get(F, member, picture, P),
	get(P, member, rdfs_hierarchy, Tree).

find(F, String:name, How:[name], In:[chain]) :->
	"Highlight nodes holding substring"::
	get(F, member, picture, P),
	get(F, tree, Tree),
	send(P, scroll_to, point(0,0)),
	send(Tree, collapse_domain),
	(   In == @default
	->  Fields = chain(label)
	;   chain_list(In, F0),
	    maplist(mkfield, F0, F1),
	    Fields =.. [chain|F1]
	),
	send(P, clear_comment),
	send(P, clear_relations),
	send(Tree, find_from, String, How, Fields).

mkfield(label,   P) :-
	rdf_global_id(rdfs:label, P).
mkfield(comment, P) :-
	rdf_global_id(rdfs:comment, P).
mkfield(synonym, P) :-			% must move to private namespace
	rdf_global_id(aat:synonym, P).

locate(F, Term:name) :->
	"Expand to given term"::
	get(F, tree, Tree),
	send(Tree, collapse_domain),
	get(Tree, add, Term, Node),
	send(Node, selected, @on),
	send(F, details, Term),
	(   get(Tree, window, Win)
	->  send(Win, normalise, Node?image)
	;   true
	).

details(F, Term:name*) :->
	"Show comments and relations"::
	(   Term == @nil
	->  send(F, clear_comment),
	    send(F, clear_relations)
	;   send(F, show_comment, Term),
	    send(F, show_relations, Term)
	).

clear_comment(F) :->
	get(F, member, view, View),
	send(View, clear).

show_comment(F, Term:name) :->
	"Show RDF comment"::
	get(F, member, view, View),
	send(View, selection, 0,0),
	(   rdf_has(Term, rdfs:comment, literal(Comment))
	->  send(View, contents, Comment?strip)
	;   send(View, contents, '<no description>')
	).

clear_relations(F) :->
	get(F, member, browser, Browser),
	send(Browser, clear).

show_relations(F, Term:name) :->
	"Show related objects"::
	rdfs_label(Term, Label),
	get(F, member, browser, Browser),
	send(Browser, clear),
	(   rdf(Term, P, Obj, _),
	    (	Obj = literal(Synonym),
		sub_property(P, _:synonym),
		Synonym \== Label
	    ->	send(Browser, append, dict_item(Synonym, style := synonym))
	    ;	(   rdf_global_id(_:identity, P)
		;   rdf_global_id(_:map_to_expert, P)
		),
		rdfs_label(Obj, IdentLabel)
	    ->	send(Browser, append, dict_item(Obj, IdentLabel,
						style := identity))
	    ;	true
	    ),
	    fail
	;   true
	).

sub_property(P, Of) :-
	rdf_global_id(Of, P), !.
sub_property(P, Of) :-
	rdf(P, rdfs:subPropertyOf, P1, _),
	sub_property(P1, Of).

open_from_browser(F, DI:dict_item) :->
	"User double-clicked in browser"::
	(   get(DI, style, identity)
	->  get(F, tree, Tree),
	    get(DI, key, Term),
	    get(Tree, add, Term, Node),
	    send(Tree, selection, @nil),
	    send(Node, selected, @on),
	    send(Tree?window, normalise, Node),
	    send(F, details, Term)
	;   get(DI, style, synonym),
	    send(F, report, warning, 'Cannot show literal')
	).
	

:- pce_end_class(rdfs_select_browser).
