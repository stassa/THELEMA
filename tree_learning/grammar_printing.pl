:-module(grammar_printing, [print_productions/0
			   ,print_grammar/0
			   ,print_compressed_corpus/0
			   ,corpus/1]).

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
	configuration:output_type(T)
	,configuration:examples_module(Es)
	,configuration:language_module(L)
	,configuration:output_file_name(grammar, O)
	,phrase(L:start, [S])
	,findall(C, Es:example_string(C), Cs)
	,corpus_productions(Cs, Ps)
	,!
	,expand_file_search_path(O, P)
	,open(P,write,Stream,[])
        ,module_name(Stream, Module_name)
	,once(print_grammar_file(T, Module_name, Stream, S, Ps))
	,close(Stream)
	,edit(P).



%!	print_compressed_corpus
%
%	Replace example strings with the symbols of nonterminals that
%	cover them in the derived grammar and print out this compressed
%	corpus.
%
print_compressed_corpus:-
	configuration:output_file_name(compressed_corpus, Compressed_corpus_file_name)
	,module_name(Compressed_corpus_file_name, Compressed_corpus_module_name)
	,configuration:output_file_name(grammar, Grammar_module_file_name)
	,use_module(Grammar_module_file_name)
	,module_name(Grammar_module_file_name, Grammar_module_name)
	,corpus(Cs)
	,expand_file_search_path(Compressed_corpus_file_name, Path)
	,open(Path,write,Stream,[close_on_abort(true)])
	% Write the module name & exports
	,format(Stream, '~w~w~w~w~w~n'
	       ,[':-module(',Compressed_corpus_module_name,','
		,[example_string/1],').'])
	,write(Stream, '\n')
	% Write compressed examples
	,forall(member(C, Cs)
		,(   phrase(Grammar_module_name:compression_grammar(Comp), C)
		 ->  print_term(Stream, p, example_string(Comp))
		 ;   true % example not covered; should we allow failures though?
		 )
	       )
	,close(Stream)
	,edit(Compressed_corpus_file_name).



%!	module_name(+Stream,-Module_name) is det.
%
%	Extract a module name from its Stream name or the value of the
%	relevant configuration option. REDUCE MOAR BOILERPLATE MOAR.
%
module_name(output(Filename), Module_name):-
	file_name_extension(Module_name, _Ext, Filename)
	,! % Backtracking to second clause raises an error
	   % since there is not stream output(Filename)
	.
% @KLUDGE
module_name(corpus(Filename), Module_name):-
	file_name_extension(Module_name, _Ext, Filename)
	,!.
module_name(Stream, Module_name):-
	stream_property(Stream, file_name(Path))
	,file_base_name(Path, Filename)
	,file_name_extension(Module_name, _Ext, Filename).


%!	print_grammar_file(+Type,+Stream,+Start,-Productions) is det.
%
%	Print a Prolog file with derived productions in DCG notation.
%	Start is the start symbol in the derived grammar; Type is the
%	value of configuration option grammar_printing/1 inherited (!)
%	from print_grammar. Something something closure Type
%	something. Made more sense when print_grammar had an argument
%	(the Type one).
%
print_grammar_file(grammar, Grammar_module_name, Stream, S, Ps):-
	% Print the module/2 statement at the start of the grammar module file.
	format(Stream, '~w~w~w~w~w~n'
	       ,[':-module(',Grammar_module_name,',',[S//0],').'])
	,write(Stream, '\n')
	,forall(member(P, Ps), print_term(Stream, p, P)).

print_grammar_file(tags, Grammar_module_name, Stream, S, Ps):-
	grammar_module_exports(Ps, [], Es)
	% We don't want to print the start symbol here.
	% @BUG
	% Hmmmrrright. But if we do it this way we miss stem-only rules like
	% ability --> [exile,target,creature]. We print those to file with
	% a new, made-up name, but they never get added to the exports.
	,once(select(S//_A, Es, Es_))
	,format(Stream, '~w~w~w' ,[':-module(',Grammar_module_name,','])
	,print_term(Stream,i,Es_)
	,format(Stream, '~w~n', [').'])
	,write(Stream, '\n')
	% Print each production unless its name is the start symbol.
	,forall(member(P-->B, Ps)
	        ,(   \+ P == S
		 ->  print_term(Stream, p, P-->B)
		 ;   B = [H|_]
		    ,print_term(Stream, p, H --> B)
		 ;   atomic(B)  % as in ability --> destroy.
		    ,B\= []
		    ,print_term(Stream, p, B --> [B])
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
		 ;   B = [H|_]
		 ->  compression_nonterminal(H --> B, N)
		    ,print_term(Stream, p, N)
		 ;   atomic(B)
		    ,B \= []
		 ->  compression_nonterminal(B--> [B], N)
		    ,print_term(Stream, p, N)
		 ;   true
		 )
	       )
	,write(Stream, '\n')
	% Print each production unless its name is the start symbol.
	,you_are_here
	,forall(member(P-->B, Ps)
	        ,(   \+ P == S
		 ->  print_term(Stream, p, P-->B)
		 ;   B = [H|_]
		    ,print_term(Stream, p, H --> B)
		 ;   atomic(B)  % as in ability --> destroy.
		    ,B\= []
		    ,print_term(Stream, p, B --> [B])
		 ;   true % probably ability --> []
		 )
	       ).

you_are_here.


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
		   (compression_grammar([A|As])-->nonterminal(A),compression_grammar(As))
		   ,[fullstop(true)
		    ,nl(true)
		    ,spacing(next_argument)
		    ,quoted(true)
		    ,variable_names(['A'=A
				    ,'As'=As])
		    ]
		   ).



%!	corpus(?Corpus) is det.
%
%	The corpus stored in the currently configured examples file.
%	Corpus is a list of token-lists.
%
corpus(Cs):-
	configuration:examples_module(Es)
	,findall(C, Es:example_string(C), Cs).
