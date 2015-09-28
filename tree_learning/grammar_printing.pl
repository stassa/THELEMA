:-module(grammar_printing, [print_productions/0
			   ,print_grammar/0]).

:-use_module(production_induction, [corpus_productions/2]).

/** <module> Predicates for printing an induced grammar.

*/

%!	print_corpus_productions is det.
%
%	Derive and print (to standard out) grammar productions learned
%	from the currently configured examples and language modules.
%
print_productions:-
	configuration:examples_module(Es)
	,findall(C, Es:example_string(C), Cs)
	,corpus_productions(Cs, Ps)
	,!
	,forall(member(P, Ps), writeln(P)).



%!	print_grammar is det.
%
%	Derive and print to the configured output file the grammar
%	learned from the currently configured etc.
%
print_grammar:-
	configuration:grammar_printing(Printing)
	,configuration:examples_module(Es)
	,configuration:language_module(L)
	,configuration:output_stream(O)
	,phrase(L:start, [S])
	,findall(C, Es:example_string(C), Cs)
	,corpus_productions(Cs, Ps)
	,!
	,expand_file_search_path(O, P)
	,open(P,write,Stream,[])
	,stream_property(Stream, file_name(Path))
	,file_base_name(Path, Filename)
	,file_name_extension(Module_name, _Ext, Filename)
	,once(print_grammar_file(Printing, Module_name, Stream, S, Ps))
	,close(Stream)
	,edit(P).


%!	print_grammar_file(+Type,+Stream,+Start,-Productions) is det.
%
%	Print a Prolog file with derived productions in DCG notation.
%	Start is the start symbol in the derived grammar; Type is the
%	value of configuration option grammar_printing/1 inherited (!)
%	from print_grammar. Something something closure Type
%	something. Made more sense when print_grammar had an argument
%	(the Type one).
%
print_grammar_file(tree, Grammar_module_name, Stream, S, Ps):-
	% Print the module/2 statement at the start of the grammar module file.
	format(Stream, '~w~w~w~w~w~n'
	       ,[':-module(',Grammar_module_name,',',[S//0],').'])
	,write(Stream, '\n')
	,forall(member(P, Ps), print_term(Stream, p, P)).

print_grammar_file(chunks, Grammar_module_name, Stream, S, Ps):-
	grammar_module_exports(Ps, [], Es)
	% We don't want to print the start symbol here.
	,once(select(S//_A, Es, Es_))
	,format(Stream, '~w~w~w' ,[':-module(',Grammar_module_name,','])
	,print_term(Stream,i,Es_)
	,format(Stream, '~w~n', [').'])
	,write(Stream, '\n')
	% Print each production unless it's name is the start symbol.
	,forall(member(P-->B, Ps)
	        ,(   \+ P == S
		 ->  print_term(Stream, p, P-->B)
		 ;   true
		 )
	       ).

/* Note: we could add the original grammar module as a reference
but that's too much trouble at this point - the moment we change
the config to print a compression grammar we lose the name of the
previously derived grammar; so we'll need a separate config option
for the compression grammar file name. Why go there? Pain. */
print_grammar_file(compression, Grammar_module_name, Stream, S, Ps):-
	% The compression grammar only needs to export nonterminal//1
	format(Stream, '~w~w~w~w~w~n'
	       ,[':-module(',Grammar_module_name,',',[compression_grammar//1,nonterminal//1],').'])
	,write(Stream, '\n')
	% Print compression_grammar//1 term; easiest thing to do to
	% resolve scope of nonterminal//1 is to print it in the same file
	,print_compression_grammar_term(Stream)
	,write(Stream, '\n')
	% Print nonterminal//1 clauses
	,forall(member(P-->B, Ps)
	        ,(   \+ P == S
		 ->  compression_nonterminal(P-->B, N)
		    ,print_term(Stream, p, N)
		 ;   true
		 )
	       )
	,write(Stream, '\n')
	% Print each production unless it's name is the start symbol.
	,forall(member(P-->B, Ps)
	        ,(   \+ P == S
		 ->  print_term(Stream, p, P-->B)
		 ;   true
		 )
	       ).


%!	print_term(+Start,+Type,+Term) is det.
%
%	convenience wrapper around write_term/3 to reduce boilerplate
%	when printing productions.
%
%	Start is the start symbol in the derived grammar; Type is one
%	of: [s,p,t,n,i] (start, production, terminal, nonterminal,
%	predicate-indicator) and determines the type of term to print;
%	Term is the term to print.
%
print_term(S, p, T):-
	write_term(S, T,[fullstop(true),nl(true),spacing(next_argument),quoted(true)]).

print_term(S, i, T):-
	write_term(S, T,[fullstop(false),spacing(next_argument),quoted(true)]).


%!	grammar_module_exports(+Productions,+Temp,-Acc) is det.
%
%	Convert between a list of productions and a list of module
%	exports as name//arity terms (for DCG rules).
%
grammar_module_exports([], Es, S_Es):-
	% To remove duplicates
	sort(Es, S_Es).
grammar_module_exports([P-->_B|Ps], Temp, Acc):-
	functor(P, F, A)
	,(   A > 0
	->  Rule_arity is A - 2 % The two extra vars added by the DCG compiler
	 ;   Rule_arity = 0
	)
	,grammar_module_exports(Ps,[F//Rule_arity|Temp], Acc).


%!	compression_nonterminal(+Production,-Nonterminal) is det.
%
%	Convert between a Production and a nonterminal//1 term that
%	maps its input to the symbol of that Production. I guess.
%
compression_nonterminal(P --> B, nonterminal(P) --> B).


%!	print_compression_grammar_term(+Stream) is det.
%
%	Print the compression predicate, used to replace tokens with the
%	names of nonterminals that cover them.
%
print_compression_grammar_term(Stream):-
	write_term(Stream, compression_grammar([]) --> []
		   ,[fullstop(true)
		    ,nl(true)
		    ,spacing(next_argument)
		    ,quoted(true)
		    ]
		   )
	,write_term(Stream,
		   (compression_grammar([A|As])-->nonterminal(A),!,compression_grammar(As))
		   ,[fullstop(true)
		    ,nl(true)
		    ,spacing(next_argument)
		    ,quoted(true)
		    ,variable_names(['A'=A
				    ,'As'=As])
		    ]
		   ).
