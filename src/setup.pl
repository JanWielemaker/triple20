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


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- [load].

win_setup :-
	current_prolog_flag(windows, true), !,
	get(new(D, win_setup_dialog), confirm_centered, _),
	send(D, destroy).
win_setup :-
	format(user_error,
	       '~nERROR: This setup file is only for Windows users.  Check the\n\
	          ERROR: documentation on the doc directory for other systems.\n', []).
	
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
	send(D, return, done),
	concat_atom(Exts, ', ', Atom),
	send(@display, inform,
	     'Registered files with the following extensions to\n\
	      start Triple20: %s',
	      Atom).

cancel(D) :->
	send(D, return, cancel).

:-pce_end_class.

%	win_register(+Ext, +Name)
%	
%	Register files of type Ext to call this program as
%	
%		plwin.exe -s load.pl -g winmain -- <file.ext>

win_register(Ext, Name) :-
	absolute_file_name(image('32x32/triple20.ico'), IconFile),
	atom_concat(IconFile, ',0', Icon),
	current_prolog_flag(executable, Exe),
	absolute_file_name(triple20(load),
			   [ file_type(prolog),
			     access(read)
			   ],
			   LoadFile),
	sformat(Command, '"~w" -s "~w" -L16m -G32m -T32m -g winmain -- "%1"',
		[Exe, LoadFile]),
	shell_register_file_type(Ext, 'triple20.type', Name, Command, Icon).

win_register(Ext) :-
	rdf_file_extension(Ext, Name),
	win_register(Ext, Name).

:- win_setup, halt.
