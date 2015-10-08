:-module(database, [record_query/2
		   ,add_record/2
		   ,delete_record/2
		   ,clear_records/2]).

/** <module> Only slightly evil interface to the dynamic database (which is
normally, like, the heart of all evil).
*/

:- dynamic covered_by/2.

%!	record_query(+Functor:atom,+Args:list) is det.
%
%	Query the dynamic database for a term:
%	==
%       database:f(a1, ..., an)
%       ==
%
%	Where f = Functor and a1, ... an = Args.
%
record_query(Functor, Args):-
	Term =.. [Functor|Args]
	,Term.



%!	add_record(+Functor:atom,+Args:list) is det.
%
%	Write to the dynamic database a term:
%	==
%	database:f(a1, ..., an)
%	==
%
%	Where f = Functor and a1, ... an = Args.
%
add_record(Functor, Args):-
	Term =.. [Functor|Args]
	,assert(Term).



%!	delete_record(+Functor:atom,+Args:list) is det.
%
%	Delete from the dynamic database the first clause of a term:
%	==
%	database:f(a1, ..., an)
%	==
%
%	Where f = Functor and a1, ... an = Args.
%
delete_record(Functor, Args):-
	Term =.. [Functor|Args]
	,retract(Term).



%!	clear_records(+Functor:atom,+Args:list) is det.
%
%
%	Delete from the dynamic database all clauses of the term:
%	==
%	database:f(a1, ..., an)
%	==
%
%	Where f = Functor and a1, ... an = Args.
%
clear_records(Functor, Args):-
	Term =.. [Functor|Args]
	,retractall(Term).
