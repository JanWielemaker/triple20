/*  File:    window.pl
    Author:  Jan Wielemaker
    Created: Jul 22 2003
    Purpose: Dedicated window classes
*/

:- module(pce_window, []).
:- use_module(library(pce)).

:- pce_begin_class(constrained_scroll_picture, picture,
		   "Picture that cannot be scrolled too far").

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

:- pce_end_class.
