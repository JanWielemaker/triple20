/*  File:    rdf_portray.pl
    Author:  Jan Wielemaker
    Created: Feb 27 2003
    Purpose: 
*/

:- module(rdf_portray, []).
:- use_module(rdfs).

:- multifile
	user:portray/1.

user:portray(URL) :-
	atom(URL),
	sub_atom(URL, 0, _, _, 'http://'),
	rdfs_ns_label(URL, Label),
	write(Label).
