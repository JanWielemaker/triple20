#!/usr/bin/swipl -s

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The driver for triple20. On Windows you   can  create a shortcut to this
file from the desktop or the startmenu. On   Unix you can update the 1st
line to reflect the path to  SWI-Prolog   and  the  triple20 search path
below to find the installation.  Then  copy   or  create  a  link from a
directory in your $PATH.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

file_search_path(triple20, 'd:/oz/Triple20'). % BJW VAIO settings
file_search_path(triple20, '/home/jan/src/Triple20/src').
:- load_files([ triple20(load)
	      ],
	      [ silent(true)
	      ]).

main :-
	current_prolog_flag(argv, Argv),
        (   append(_, [--|Av], Argv)
	->  triple20(Av)
	;   triple20([])
	).

:- main.
