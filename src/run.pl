#!/staff/jan/bin/pl -G32m -T32m -L16m -s

:- [load].

main :-
	current_prolog_flag(argv, Argv),
        append(_, [--|Av], Argv),
	go(Av).

:- main.
