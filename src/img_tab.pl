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

:- module(img_tab, []).
:- use_module(library(pce)).
:- use_module(rdf_rules).
:- use_module(library(scaledbitmap)).
:- use_module(library(url_image)).


		 /*******************************
		 *	    IMAGE WINDOW	*
		 *******************************/

:- pce_begin_class(image_window, window,
		   "Show image URL in a window").

variable(resource,	name*, get, "Displayed window").

initialise(IW, Resource:[name]*) :->
	send_super(IW, initialise),
	(   atom(Resource)
	->  send(IW, resource, Resource)
	;   true
	).


resource(IW, Resource:name*) :->
	"Dislay a resource"::
	(   get(IW, resource, Resource)
	->  true
	;   send(IW, clear),
	    (   Resource == @nil
	    ->  true
	    ;	call_rules(IW, image(Resource, Img))
	    ->	send(Img, name, image),
		send(IW, display, Img),
		send(IW, resize)
	    ;	true
	    ),
	    send(IW, slot, resource, Resource)
	).


value(IW, Resource:name*) :->
	"Triple20 integration"::
	send(IW, resource, Resource).


resize(IW) :->
	"Scale and center the image"::
	(   get(IW, member, image, BM)
	->  get(IW, visible, Visible),
	    send(BM, size, Visible?size),
	    send(BM, center, Visible?center)
	;   true
	).

:- pce_end_class(image_window).
