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

:- module(rdfs_resource_item, []).
:- use_module(library(pce)).
:- use_module(library(lists)).
:- use_module(rdfs_hierarchy).
:- use_module(semweb(rdf_db)).
:- use_module(semweb(rdfs)).
:- use_module(owl).
:- use_module(rdf_dialog).
:- use_module(rdf_rules).

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
	send(OI, border, size(12,6)),	% try better layout BJW, was 10, 5
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
	send(new(B, t20_select_browser(OI)), open),
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
	    ;   send(B, find, Typed, prefix)
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
	;   call_rules(OI, label_text(Term, Label)),
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
	    (	(   owl_satisfies(Domain, Term)
		;   rdfs_individual_of(Term, rdfs:'Class'),
		    owl_satisfies(Domain, individual_of(Term))
		)
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
	rdf_has(Resource, rdfs:label, literal(prefix(Prefix), Label0)),
	label_text(Label0, Label),
	owl_satisfies(Domain, Resource).

label_text(lang(_, Label), Label) :- !.
label_text(Label, Label).

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

:- pce_begin_class(t20_select_browser, frame,
		   "Browse contology tree for selecting").

initialise(F, Selector:rdfs_resource_item) :->
	"Create for selector"::
	send_super(F, initialise, 'Select term'),
	send(F, append, new(D1, rdf_search_dialog)),
	send(D1, append, button(cancel, message(F, destroy)), right),
	send(D1, name, top),
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

tree(F, Tree:rdfs_tree) :<-
	"Get the tree object"::
	get(F, member, picture, P),
	get(P, member, rdfs_hierarchy, Tree).

find(F, String:name, How:[name], Fields:[chain|{*}]) :->
	"Highlight nodes search"::
	get(F, member, picture, P),
	get(F, tree, Tree),
	send(P, scroll_to, point(0,0)),
	send(Tree, collapse_domain),
	send(P, clear_comment),
	send(P, clear_relations),
	send(Tree, find_from, String, How, Fields).

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
	findall(Label, rdfs_label(Term, Label), Labels),
	list_to_set(Labels, [Label|Synonyms]),
	get(F, member, browser, Browser),
	send(Browser, clear),
	(   member(Synonym, Synonyms),
	    send(Browser, append, dict_item(Synonym, style := synonym)),
	    fail
	;   true
	),
	(   rdf_has(Term, owl:sameAs, Obj),
	    rdfs_ns_label(Obj, IdentLabel),
	    send(Browser, append, dict_item(Obj, IdentLabel,
					    style := identity)),
	    fail
	;   true
	).

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
	

:- pce_end_class(t20_select_browser).
