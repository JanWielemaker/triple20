#!/staff/jan/bin/pl -G32m -T32m -L16m -s

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The driver for triple20. On Windows you   can  create a shortcut to this
file from the desktop or the startmenu. On   Unix you can update the 1st
line to reflect the path to  SWI-Prolog   and  the  triple20 search path
below to find the installation.  Then  copy   or  create  a  link from a
directory in your $PATH.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

file_search_path(triple20, '.').
file_search_path(triple20, '/staff/jan/projects/mia/jan/semweb').

:- [triple20(load)].

main :-
	current_prolog_flag(argv, Argv),
        append(_, [--|Av], Argv),
	go(Av).

:- main.
