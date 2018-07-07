#!/usr/bin/env swipl

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The driver for triple20. On Windows you   can  create a shortcut to this
file from the desktop or the startmenu. On   Unix you can update the 1st
line to reflect the path to  SWI-Prolog   and  the  triple20 search path
below to find the installation.  Then  copy   or  create  a  link from a
directory in your $PATH.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

file_search_path(triple20, '/home/jan/src/prolog/triple20/src').
:- load_files([ triple20(load)
	      ],
	      [ silent(true)
	      ]).

main :-
	current_prolog_flag(argv, Argv),
	triple20(Argv).

:- initialization(main, main).
