:- [rdfs_collection_item].

t :-
	new(LB, rdf_list_browser('List__1')),
	new(P, picture),
	send(P, display, LB),
	send(P, open).

:- pce_begin_class(foo, object).

write(F) :->
	"Nice comment"::
	writeln(F).

:- pce_end_class(foo).
