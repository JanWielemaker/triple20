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

:- module(xml_namespaces,
	  [ xml_namespace/2,		% ?Id, ?URL
	    xml_namespace_element/2,	% ?NS, ?Element
	    xml_namespace_attribute/2	% ?NS, ?Attribute
	  ]).

:- multifile
	xml_namespace/2,
	xml_namespace_element/2,
	xml_namespace_attribute/2.

%	xml_namespace(?Id, ?URL)
%	
%	Table to deal with registered namespaces

xml_namespace(rdf,  'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
xml_namespace(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').
xml_namespace(xsd,  'http://www.w3.org/2000/10/XMLSchema#').
xml_namespace(owl,  'http://www.w3.org/2002/07/owl#').
xml_namespace(dc,   'http://purl.org/dc/elements/1.1/').


%	xml_namespace_member(?Id, ?Token)
%	
%	Enumerate the tokes that may appear in a namespace. This is
%	primarily used to produce sensible error messages.


		 /*******************************
		 *               RDF		*
		 *******************************/

xml_namespace_element(rdf, 'RDF').
xml_namespace_element(rdf, 'Property').

xml_namespace_attribute(rdf, 'about').
xml_namespace_attribute(rdf, 'ID').
xml_namespace_attribute(rdf, 'nodeID').
xml_namespace_attribute(rdf, 'bagID').
xml_namespace_attribute(rdf, 'resource').
xml_namespace_attribute(rdf, 'type').


		 /*******************************
		 *	       RDFS		*
		 *******************************/


xml_namespace_element(rdfs, 
