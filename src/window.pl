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

:- module(pce_window, []).
:- use_module(library(pce)).
:- use_module(library(print_graphics)).
:- use_module(rdf_rules).

:- pce_begin_class(constrained_scroll_picture, picture,
		   "Picture that cannot be scrolled too far").
:- use_class_template(rdf_arm).
:- use_class_template(drop_files).
:- use_class_template(print_graphics).

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


resource(_, _) :<-
	"Virtual method"::
	fail.

print_graphics(P) :->
	"More unique name"::
	send(P, print).

popup(W, P:popup) :<-
	call_rules(W, popup(W, P)).

:- pce_end_class.
