/*  File:    rdf_edit.pl
    Author:  Jan Wielemaker
    Created: Feb  2 2003
    Purpose: Modification interface
*/

:- module(rdf_edit,
	  [ rdfe_assert/3,		% Sub, Pred, Obj
	    rdfe_retractall/3,		% Sub, Pred, Obj
	    rdfe_update/4,		% Sub, Pred, Obj, +Action
	    rdfe_load/1,		% +File
	    rdfe_delete/1,		% +Resource

	    rdfe_transaction/1,		% :Goal

	    rdfe_undo/0,		% 
	    rdfe_redo/0,
	    rdfe_can_undo/0,
	    rdfe_can_redo/0,

	    rdfe_open_journal/2,	% +File, +Mode
	    rdfe_close_journal/0,
	    rdfe_replay_journal/1,	% +File
	    rdfe_current_journal/1	% -Path
	  ]).
:- use_module(library(rdf_db)).
:- use_module(library(broadcast)).

:- meta_predicate
	rdfe_transaction(:).

:- dynamic
	undo_log/5,			% TID, Action, Subj, Pred, Obj
	current_transaction/1,		% TID
	undo_marker/2,			% Mode, TID
	journal/2.			% Path, Stream

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This library provides a number of functions on top of the rdf_db module:

    * Broadcast modifications
    * Provide undo/redo
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%	user:goal_expansion(+NSGoal, -Goal)
%	
%	This predicate allows for writing down rdf queries in a friendly
%	name-space fashion.  

:- multifile
	user:goal_expansion/2.

user:goal_expansion(rdfe_assert(Subj0, Pred0, Obj0),
		    rdfe_assert(Subj, Pred, Obj)) :-
	rdf_global_id(Subj0, Subj),
	rdf_global_id(Pred0, Pred),
	rdf_global_id(Obj0, Obj).
user:goal_expansion(rdfe_retract(Subj0, Pred0, Obj0),
		    rdfe_retract(Subj, Pred, Obj)) :-
	rdf_global_id(Subj0, Subj),
	rdf_global_id(Pred0, Pred),
	rdf_global_id(Obj0, Obj).
user:goal_expansion(rdfe_update(Subj0, Pred0, Obj0, Action0),
		    rdfe_update(Subj, Pred, Obj, Action)) :-
	rdf_global_id(Subj0, Subj),
	rdf_global_id(Pred0, Pred),
	rdf_global_id(Obj0, Obj),
	(   compound(Action0)
	->  Action0 =.. [Name,Res0],
	    rdf_global_id(Res0, Res),
	    Action =.. [Name,Res]
	;   Action = Action0
	).
user:goal_expansion(rdfe_delete(Subj0),
		    rdfe_delete(Subj)) :-
	rdf_global_id(Subj0, Subj).
user:goal_expansion(rdfe_transaction(G0),
		    rdfe_transaction(G)) :-
	expand_goal(G0, G).


		 /*******************************
		 *     BASIC EDIT OPERATIONS	*
		 *******************************/

rdfe_assert(Subject, Predicate, Object) :-
	rdfe_assert(Subject, Predicate, Object, user).

rdfe_assert(Subject, Predicate, Object, PayLoad) :-
	rdf_assert(Subject, Predicate, Object, PayLoad),
	rdfe_current_transaction(TID),
	assert_action(TID, assert(PayLoad), Subject, Predicate, Object),
	journal(assert(TID, Subject, Predicate, Object, PayLoad)).

rdfe_retractall(Subject, Predicate, Object) :-
	rdfe_retractall(Subject, Predicate, Object, _).

rdfe_retractall(Subject, Predicate, Object, PayLoad) :-
	rdf_retractall(Subject, Predicate, Object, PayLoad),
	rdfe_current_transaction(TID),
	assert_action(TID, retract(PayLoad), Subject, Predicate, Object),
	journal(retract(TID, Subject, Predicate, Object, PayLoad)).
	
%	rdfe_update(+Subject, +Predicate, +Object, +Action)
%	
%	Update an existing triple.  Possible actions are:
%	
%		subject(+Subject)
%		predicate(+Predicate)
%		object(+Object)
%		
%	Broadcast message:
%	
%		rdf(+Action, +Subject, +Predicate, Object)

rdfe_update(Subject, Predicate, Object, Action) :-
	rdfe_current_transaction(TID),
	rdf_update(Subject, Predicate, Object, Action),
	(   Action = object(New)
	->  assert_action(TID, object(Object), Subject, Predicate, New)
	;   Action = predicate(New)
	->  assert_action(TID, predicate(Predicate), Subject, New, Object)
	;   Action = subject(New)
	->  assert_action(TID, subject(Subject), New, Predicate, Object)
	),
	journal(update(TID, Subject, Predicate, Object, Action)).

%	rdfe_delete(+Subject)
%	
%	Delete a subject and all we know about it. This is a bit tricky.
%	If we are involved in transitive relations, should we re-joint
%	these in this module?

rdfe_delete(Subject) :-
	rdfe_transaction(delete(Subject)).

delete(Subject) :-
	rdfe_retractall(Subject, _, _),
	rdfe_retractall(_, Subject, _),
	rdfe_retractall(_, _, Subject).

%	rdfe_load(+File)
%	
%	Load an RDF file and record this action including version information
%	to facilitate reliable reload.
%
%	TBD: Add this to undo mechanism

rdfe_load(File) :-
	rdfe_current_transaction(TID),
	rdf_statistics(triples(Old)),
	rdf_load(File),
	rdf_statistics(triples(New)),
	Loaded is New - Old,
	absolute_file_name(File, Path),
	absolute_file_name('.', PWD),
	size_file(File, Size),
	time_file(File, Modified),
	SecTime is round(Modified),
	journal(rdf_load(TID,
			 Path,
			 [ pwd(PWD),
			   size(Size),
			   modified(SecTime),
			   triples(Loaded)
			 ])).


		 /*******************************
		 *	   TRANSACTIONS		*
		 *******************************/

%	rdfe_transaction(:Goal)
%	
%	Run Goal, recording all modifications   as a single transaction.
%	If  Goal  raises  an  exception  or    fails,  all  changes  are
%	rolled-back.

rdfe_transaction(Goal) :-
	rdfe_begin_transaction,
	(   catch(Goal, E, true)
	->  (   var(E)
	    ->	rdfe_commit
	    ;	rdfe_rollback,
		throw(E)
	    )
	;   rdfe_rollback,
	    fail
	).
	    
%	rdfe_begin_transaction/0
%	
%	Start a transaction.  This is followed by either rdfe_end_transaction
%	or rdfe_rollback.  Transactions may be nested.

rdfe_begin_transaction :-
	current_transaction(TID), !,	% nested transaction
	append(TID, [1], TID2),
	asserta(current_transaction(TID2)).
rdfe_begin_transaction :-		% toplevel transaction
	flag(rdf_edit_tid, TID, TID+1),
	asserta(current_transaction([TID])).

rdfe_current_transaction(TID) :-
	current_transaction(TID), !.
rdfe_current_transaction(_) :-
	throw(error(existence_error(rdf_transaction, _), _)).

rdfe_commit :-
	retract(current_transaction(TID)), !,
	journal(commit(TID)),
	(   TID = [Id]
	->  broadcast(rdf_transaction(Id))
	;   true
	).

rdfe_rollback :-
	retract(current_transaction(TID)), !,
	journal(rollback(TID)),
	undo(TID).

assert_action(TID, Action, Subject, Predicate, Object) :-
	asserta(undo_log(TID, Action, Subject, Predicate, Object)).

%	undo(+TID)
%	
%	Undo a transaction as well as possible transactions nested into
%	it.

undo(TID) :-
	append(TID, _, Id),
	(   retract(undo_log(Id, Action, Subject, Predicate, Object)),
	    (	undo(Action, Subject, Predicate, Object)
	    ->	fail
	    ;	print_message(warning,
			      rdf_undo_failed(undo(Action, Subject,
						   Predicate, Object))),
		fail
	    )
	;   true
	).
	
undo(assert(PayLoad), Subject, Predicate, Object) :- !,
	rdfe_retractall(Subject, Predicate, Object, PayLoad).
undo(retract(PayLoad), Subject, Predicate, Object) :- !,
	rdfe_assert(Subject, Predicate, Object, PayLoad).
undo(Action, Subject, Predicate, Object) :-
	action(Action), !,
	rdfe_update(Subject, Predicate, Object, Action).

action(subject(_)).
action(predicate(_)).
action(object(_)).

%	rdfe_undo
%	
%	Undo a (toplevel) transaction. More calls do further undo. The
%	`Undone' actions are re-added to the undo log, so the user can
%	redo them.  Fails if there are no more undo/redo transactions.

rdfe_undo :-
	undo_marker(undo, TID), !,
	(   undo_previous(TID, UnDone)
	->  retract(undo_marker(undo, TID)),
	    assert(undo_marker(undo, UnDone)),
	    broadcast(rdf_undo(undo, UnDone))
	;   fail			% start of undo log
	).
rdfe_undo :-
	retract(undo_marker(redo, _)), !,
	last_transaction(TID),
	undo_previous(TID, UnDone),
	assert(undo_marker(undo, UnDone)),
	broadcast(rdf_undo(undo, UnDone)).
rdfe_undo :-
	last_transaction(TID),
	undo_previous(TID, UnDone),
	assert(undo_marker(undo, UnDone)),
	broadcast(rdf_undo(undo, UnDone)).

find_previous_undo(-1, _) :- !,
	fail.
find_previous_undo(TID, TID) :-
	undo_log([TID|_], _, _, _, _), !.
find_previous_undo(TID0, TID) :-
	TID1 is TID0 - 1,
	find_previous_undo(TID1, TID).

undo_previous(TID, Undone) :-
	find_previous_undo(TID, Undone),
	rdfe_transaction(undo([Undone])).

last_transaction(TID) :-
	undo_log([TID|_], _, _, _, _), !.

%	rdfe_redo
%	
%	Start a redo-session

rdfe_redo :-
	(   retract(undo_marker(undo, _))
	->  last_transaction(TID),
	    undo_previous(TID, UnDone),
	    assert(undo_marker(redo, UnDone))
	;   retract(undo_marker(redo, TID))
	->  undo_previous(TID, UnDone),
	    assert(undo_marker(redo, UnDone))
	;   true
	),
	broadcast(rdf_undo(redo, UnDone)).


rdfe_can_redo :-
	undo_marker(undo, _), !.
rdfe_can_redo :-
	undo_marker(redo, TID),
	find_previous_undo(TID, _).

rdfe_can_undo :-
	undo_marker(undo, TID), !,
	find_previous_undo(TID, _).
rdfe_can_undo :-
	undo_log(_, _, _, _, _).

	
		 /*******************************
		 *	    JOURNALLING		*
		 *******************************/

journal_version(1).

rdfe_open_journal(_, _) :-		% already open
	journal(_, _), !.
rdfe_open_journal(File, Mode) :-
	absolute_file_name(File,
			   [ extensions([rdfj, '']),
			     access(write)
			   ],
			   Path),
	(   Mode == append,
	    exists_file(Path)
	->  Start = resume(Options),
	    rdfe_replay_journal(File)
	;   Start = start(Options)
	),
	open(Path, Mode, Stream, [close_on_abort(false)]),
	assert(journal(Path, Stream)),
	(   Start = start(_)
	->  format(Stream,
		   '/* RDF editor journal\n\n   \
		   Created by XPCE/SWI-Prolog RDF editor\n   \
		   By Jan Wielemaker <jan@swi.psy.uva.nl>\n\n   \
		   Do not edit!\n\
		    */~n~n', [])
	;   true
	),
	get_time(T),
	SecTime is round(T),
	journal_version(Version),
	Options = [ time(SecTime),
		    version(Version)
		  ],
	journal(Start),
	at_halt(ignore(rdfe_close_journal)).

rdfe_close_journal :-
	get_time(T),
	SecTime is round(T),
	journal(end([ time(SecTime)
		    ])),
	retract(journal(_, Stream)),
	close(Stream).

%	rdfe_current_journal(-Path)
%	
%	Query the currently open journal

rdfe_current_journal(Path) :-
	journal(Path, _Stream).

journal(Term) :-
	(   journal(_, Stream)
	->  format(Stream, '~q.~n', [Term]),
	    flush_output(Stream)
	;   report_no_journal
	).

:- dynamic
	reported_no_journal/0.

report_no_journal :-
	reported_no_journal.
report_no_journal :-
	new(D, dialog('No project')),
	send(D, append,
	     text('No project has created or opened.  Your modifications\n\
	           are not saved unless you create or open a project.')),
	send(D, append, button('Ok, all data will be lost',
			       message(D, return, ok))),
	send(D, append, button('Remind me next time',
			       message(D, return, remind))),
	get(D, confirm_centered, RVal),
	send(D, destroy),
	(   RVal == ok
	->  assert(reported_no_journal)
	;   true
	).
	     

%	rdfe_replay_journal(+File)
%	
%	Replay a journal file. For now  this   is  our cheap way to deal
%	with save/load. Future versions may be  more clever when dealing
%	with the version information stored in the journal.

rdfe_replay_journal(File) :-
	absolute_file_name(File,
			   [ extensions([rdfj, '']),
			     access(read)
			   ],
			   Path),
	open(Path, read, Stream),
	replay(Stream),
	close(Stream).

replay(Stream) :-
	read(Stream, Term),
	replay(Term, Stream).

replay(end_of_file, _) :- !.
replay(start(_Attributes), Stream) :- !,
	read(Stream, Term),
	replay(Term, Stream).
replay(resume(_Attributes), Stream) :- !,
	read(Stream, Term),
	replay(Term, Stream).
replay(end(_Attributes), Stream) :- !,
	read(Stream, Term),
	replay(Term, Stream).
replay(Term0, Stream) :-
	replay_transaction(Term0, Stream),
	read(Stream, Term),
	replay(Term, Stream).

replay_transaction(Term0, Stream) :-
	collect_transaction(Term0, Stream, Transaction, Last),
	(   Last = commit(_)
	->  replay_actions(Transaction)
	;   true
	).

collect_transaction(End, _, [], End) :-
	ends_transaction(End), !.
collect_transaction(A, Stream, [A|T], End) :-
	read(Stream, Term),
	collect_transaction(Term, Stream, T, End).

ends_transaction(end_of_file).
ends_transaction(commit(_)).
ends_transaction(rollback(_)).
ends_transaction(end(_)).
ends_transaction(start(_)).

replay_actions([]).
replay_actions([H|T]) :-
	(   replay_action(H)
	->  true
	;   print_message(warning,
			  rdf_replay_failed(H))
	),
	replay_actions(T).

replay_action(retract(_, Subject, Predicate, Object, PayLoad)) :-
	rdf_retractall(Subject, Predicate, Object, PayLoad).
replay_action(assert(_, Subject, Predicate, Object, PayLoad)) :-
	rdf_assert(Subject, Predicate, Object, PayLoad).
replay_action(update(_, Subject, Predicate, Object, Action)) :-
	rdf_update(Subject, Predicate, Object, Action).
replay_action(rdf_load(_, File, Options)) :-
	find_file(File, Options, Path),
	rdf_load(Path).

find_file(File, _, File) :-
	exists_file(File), !.
find_file(File, Options, Path) :-
	memberchk(pwd(PWD), Options),
	make_path(File, PWD, Path),
	exists_file(Path), !.

%	make_path(+File, +PWD, -Path)
%	
%	Return location of File relative to PWD, Parent of PWD, etc. (TBD)

make_path(File, PWD, Path) :-
	atom_concat(PWD, /, PWD2),
	atom_concat(PWD2, Path, File).


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3,
	user:message_hook/3.

%	Catch messages.  sgml/4 is generated by the SGML2PL binding.

prolog:message(rdf_replay_failed(Term)) -->
	[ 'RDFDB: Replay of ~p failed'-[Term] ].
prolog:message(rdf_undo_failed(Term)) -->
	[ 'RDFDB: Undo of ~p failed'-[Term] ].
