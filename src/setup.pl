/*  File:    winsetup.pl
    Author:  jan
    Created: Sep 24 2003
    Purpose: Register the tool with the MS-Windows shell
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- [load].

win_setup :-
	get(new(D, win_setup_dialog), confirm_centered, _),
	send(D, destroy).
	
:- pce_begin_class(win_setup_dialog, dialog).

initialise(D) :->
	send_super(D, initialise, 'Register file-types'),
	send(D, append,
	     label(help,
		   'Select file-types to associate to this application')),
	send(D, append, new(M, menu(extensions, toggle))),
	send_list(M, append,
		  [ rdfj,
		    rdf,
		    rdfs,
		    owl
		  ]),
	send(M, selection, rdfj),
	send(D, append, button(register)),
	send(D, append, button(cancel)).

register(D) :->
	get(D, member, extensions, M),
	get_chain(M, selection, Exts),
	checklist(win_register, Exts),
	send(D, return, done).

cancel(D) :->
	send(D, return, cancel).

:-pce_end_class.

%	win_register(+Ext, +Name)
%	
%	Register files of type Ext to call this program as
%	
%		plwin.exe -s load.pl -g winmain -- <file.ext>

win_register(Ext, Name) :-
	absolute_file_name(image('32x32/owl.ico'), IconFile),
	atom_concat(IconFile, ',0', Icon),
	current_prolog_flag(executable, Exe),
	absolute_file_name(ontoshow(load),
			   [ file_type(prolog),
			     access(read)
			   ],
			   LoadFile),
	sformat(Command, '"~w" -s "~w" -g winmain -- "%1"',
		[Exe, LoadFile]),
	shell_register_file_type(Ext, 'ontoshow.type', Name, Command, Icon).

win_register(Ext) :-
	rdf_file_extension(Ext, Name),
	win_register(Ext, Name).


