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

:- module(concur,
	  [ concurrent/2
	  ]).

:- meta_predicate
	concurrent(+, :).

%	concurrent(+N, :Goals)
%	
%	Run Goals in parallel using N threads. This call blocks until
%	all work has been done.

concurrent(1, Goals) :- !,
	checklist(call, Goals).
concurrent(N, Goals) :-
	strip_module(Goals, M, List),
	message_queue_create(Done),
	message_queue_create(Queue),
	create_workers(N, M, Queue, Done),
	forall(member(G, List),
	       thread_send_message(Queue, goal(G))),
	wait_l(List, Done),
	forall(between(1, N, _),
	       thread_send_message(Queue, done)),
	wait_n(N, Done),
	message_queue_destroy(Queue),
	message_queue_destroy(Done).

wait_l([], _).
wait_l([_|T], Queue) :-
	thread_get_message(Queue, done),
	wait_l(T, Queue).

wait_n(0, _) :- !.
wait_n(N, Queue) :-
	thread_get_message(Queue, done),
	N2 is N - 1,
	wait_n(N2, Queue).

create_workers(N, Module, Queue, Done) :-
	N > 0, !,
	thread_create(worker(Module, Queue, Done),
			     _,
			     [ detached(true)
			     ]),
	N2 is N - 1,
	create_workers(N2, Module, Queue, Done).
create_workers(_, _, _, _).


worker(Module, Queue, Done) :-
	thread_get_message(Queue, Message),
	(   Message = goal(Goal)
	->  (   catch(Module:Goal, E, true)
	    ->	(   var(E)
		->  true
		;   print_message(error, E)
		)
	    ;	print_message(warning, failed(Goal))
	    ),
	    thread_send_message(Done, done),
	    worker(Module, Queue, Done)
	;   thread_send_message(Done, done)
	).

