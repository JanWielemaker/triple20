/*  File:    pcecall.pl
    Author:  Jan Wielemaker
    Created: Aug 22 2003
    Purpose: 
*/

:- module(pce_call,
	  [ in_pce_thread/1		% :Goal
	  ]).
:- initialization
   load_foreign_library(pcecall).
