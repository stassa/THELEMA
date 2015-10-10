:-module(grammar_printing, [print_productions/0
			   ,print_grammar/0
			   ,print_compressed_corpus/0
			   ]).

:-use_module(production_induction, [corpus_productions/2]).
:-use_module(project_root(utilities), [examples_corpus/1]).


/** <module> Predicates for printing an induced grammar.

*/

%!	print_corpus_productions is det.
%
%	Derive and print (to standard out) grammar productions learned
%	from the currently configured examples and language modules.
%
print_productions:-
	examples_corpus(Cs)
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
	,configuration:language_module(L)
	,configuration:output_file_name(grammar, O)
	,phrase(L:start, [S])
	,examples_corpus(Cs)
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
	,examples_corpus(Cs)
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
print_grammar_file(dcg, Grammar_module_name, Stream, S, Ps):-
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

/* Good old-fashioned BNF; not used much these days. */
print_grammar_file(Type, _, Stream, S, Ps):-
	Type = bnf
	% Print the module/2 statement at the start of the grammar module file.
	,forall(member(P, Ps),
		(   % Top-level term
		    P = (S --> N)
		->  sanitise_bnf_identifier(S, S_san)
		   ,atomic_list_concat([<, S_san, >], '', S_)
		   ,sanitise_bnf_identifier(N, N_san)
		   ,atomic_list_concat([<, N_san, >], '', N_)
		   ,atomic_list_concat([S_, ::=, N_], ' ', Ls)
		   ,print_term(Stream, Type, Ls)
		    % Restricted GNF nonterminal:
		;   P = (Ph --> [T], N)
		->  sanitise_bnf_identifier(Ph, Ph_san)
		   ,atomic_list_concat([<, Ph_san, >], '', Ph_)
		   ,sanitise_bnf_identifier(N, N_san)
		   ,atomic_list_concat([<, N_san, >], '', N_)
		   ,atomic_list_concat([Ph_, ::=, T, N_], ' ', Ls)
		   ,print_term(Stream, Type, Ls)
		    % Restricted GNF preterminal
		;   P = (Ph --> [T])
		-> sanitise_bnf_identifier(Ph, Ph_san)
		   ,atomic_list_concat([<, Ph_san, >], '', Ph_)
		   ,atomic_list_concat([Ph_, ::=, T], ' ', Ls)
		   ,print_term(Stream, Type, Ls)
		)
	       ).

/* Print the grammar in ebnf format. Like BNF but extended. With, um. More stuff. */
print_grammar_file(Type, _, Stream, S, Ps):-
	Type = ebnf
	,forall(member(P, Ps),
		(   % Top-level term
		    P = (S --> N)
		->  sanitise_bnf_identifier(S, S_san)
		   ,sanitise_bnf_identifier(N, N_san)
		   ,atomic_list_concat([S_san, ::=, N_san], ' ', Ls)
		   ,print_term(Stream, Type, Ls)
		    % Restricted GNF nonterminal:
		;   P = (Ph --> [T], N)
		->  sanitise_bnf_identifier(T, T_san)
		   ,atomic_list_concat(["'",T_san,"'"], T_)
		   ,sanitise_bnf_identifier(Ph, Ph_san)
		   ,sanitise_bnf_identifier(N, N_san)
		   ,atomic_list_concat([Ph_san, ::=, T_, N_san], ' ', Ls)
		   ,print_term(Stream, Type, Ls)
		    % Restricted GNF preterminal
		;   P = (Ph --> [T])
		->  sanitise_bnf_identifier(T, T_san)
		   ,atomic_list_concat(["'",T_san,"'"], T_)
		   ,sanitise_bnf_identifier(Ph, Ph_san)
		   ,atomic_list_concat([Ph_san, ::=, T_], ' ', Ls)
		   ,print_term(Stream, Type, Ls)
		)
	       ).

/* Print the grammar in dot-language format used to visualise it with GraphViz. */
print_grammar_file(Type, _, Stream, S, Ps):-
	Type = dot
	% Print start of graph statement
	,format(Stream, '~w ~w ~w ~n', ['strict digraph', S, '{'])
	,writeln(Stream, 'ordering="out";\n')
	,forall(member(P, Ps),
		(   % Top-level term
		    P = (S --> N)
		->  print_dot_language_node(n, Stream, S, S, circle)
		   ,print_dot_language_edge(n,Stream, S, N, [])
		    % Restricted GNF nonterminal:
		;   P = (Ph --> [T], N)
		->  % Prefix terminal name with "t_"
		    % to distinguish from synonymous nonterminal
		    print_dot_language_node(n, Stream, Ph, Ph, diamond)
		   ,print_dot_language_node(t, Stream, T, T, box)
		   ,print_dot_language_edge(t,Stream, Ph, T,N)
		    % Restricted GNF preterminal
		;   P = (Ph --> [T])
		->
		    print_dot_language_node(p, Stream, Ph, Ph, ellipse)
		   ,print_dot_language_node(t, Stream, T, T, box)
		   ,print_dot_language_edge(p, Stream, Ph, T, [])
		)
	       )
	,writeln(Stream, '}').

/* As dot, but with fewer terminals */
print_grammar_file(Type, _, Stream, S, Ps):-
	Type = lean_dot
	% Print start of graph statement
	,format(Stream, '~w ~w ~w ~n', ['strict digraph', S, '{'])
	,writeln(Stream, 'ordering="out";\n')
	,writeln(Stream, 'node [shape = "box"];\n')
	,forall(member(P, Ps),
		(   P = (S --> N)
		->  print_dot_language_node(n, Stream, S, S, doublecircle)
		   ,print_dot_language_edge(n,Stream, S, N, [])
		;   P = (Ph --> [T], N)
		->  print_dot_language_node(n, Stream, Ph, Ph, ellipse)
		   ,print_dot_language_edge(n,Stream, Ph, N, [])
		    % Don't print out preterminals explicitly.
		    % We will print an edge to them from a nonterminal.
		;   P = (Ph --> [T])
		->  true
		)
	       )
	,writeln(Stream, '}').



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

print_term(S, Type, T):-
	(   Type = bnf
	;   Type = ebnf
	)
	% If the term is lexicalised, concatentate its constituents.
	,format(S, '~w~n', T).



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



%!	sanitise_bnf_identifier(+Identifier,-Sanitised) is det.
%
%	Sanitise identifiers for output_type/1 = [bnf,ebnf]. Mostly
%	makes sure to convert lexicalised forms to concatenated, for
%	example converting "destroy(target)" to "destroy_target".
%
sanitise_bnf_identifier(T, T_):-
	T =.. Params
	,atomic_list_concat(Params, '_', T_).



%!	print_dot_language_node(+Type,+Stream,+Node,+Label,+Shape) is det.
%
%	Print a node statement in dot-language to the given Stream.
%	Clauses are selected according to Type, which can be one of: n
%	(for a nonterminal node), t (terminal) or p (a pre-terminal).
%
print_dot_language_node(T, Stream, Node, Label, Shape):-
	sanitise_dot_language_term(Node, Node_)
	,sanitise_dot_language_term(Label, Label_)
	% This is wrong and you'll burn in hell for this:
	,print_dot_language_node_(T, Stream, Node_, Label_, Shape).

print_dot_language_node_(n, Stream, Node, Label, Shape):-
	format(Stream, '"~w" ~w "~w"~w ~w~w~w~n', [Node,'[label =',Label,',','shape = "',Shape,'"];']).

print_dot_language_node_(t, Stream, Node, Label, Shape):-
	format(Stream, '"~w~w" ~w "~w"~w ~w~w~w~n', [t_,Node,'[label =',Label,',','shape = "',Shape,'"];']).

print_dot_language_node_(p, Stream, Node, Label, Shape):-
	format(Stream, '"~w" ~w "~w"~w ~w~w~w~n', [Node,'[label =',Label,',','shape = "',Shape,'"];']).


%!	print_dot_language_edge(+Type,+Stream,+Head,+Synonym,+Tail) is det.
%
%	Print an edge statement in dot-language to the given Stream.
%	Clauses are selected according to Type which can be one of: n
%	(nonterminal), t (terminal) or p (pre-terminal).
%
%	Head, Synonym and Tail are the constituents of a rule in GNF
%	format:
%	==
%	H --> S, T.
%	==
%
%	Where S = [H]
print_dot_language_edge(T, Stream, Head, Synonym, Refs):-
	sanitise_dot_language_term(Head, Head_)
	,sanitise_dot_language_term(Synonym, Synonym_)
	,sanitise_dot_language_term(Refs, Refs_)
	,print_dot_language_edge_(T, Stream, Head_, Synonym_, Refs_).

% Really bad practice- don't do that.
print_dot_language_edge_(n, Stream, Head, Synonym, []):-
	format(Stream, '"~w" ~w "~w"~w~n~n', [Head, ->, Synonym, ;]).

print_dot_language_edge_(n, Stream, Head, Synonym, _Refs):-
	format(Stream, '"~w" ~w "~w"~w~n~n', [Head, ->, Synonym, ;]).

print_dot_language_edge_(t, Stream, Head, Synonym, Refs):-
	format(Stream, '"~w" ~w "~w~w"~w "~w"~w~n~n', [Head, ->, t_, Synonym, ',', Refs, ;]).

print_dot_language_edge_(p, Stream, Head, Synonym, []):-
	format(Stream, '"~w" ~w "~w~w"~w~n~n', [Head, ->, t_,Synonym, ;]).


%!	sanitise_dot_language_term(+Term, -Sanitised) is det.
%
%	Avoid dot-language escape sequences. Mostly replaces
%	double-quotes with the atom "double_quotes" and the single quote
%	with the atom "single_quote".
%
%	The reason we do that (and also the horrible non-underscored -
%	calls-underscored thingy above) is that dot language escapes
%	only one character, the double-quote, and it escapes it with a
%	single quote. Which inevitably leads to a big old mess when we
%	try to print out dot language from Prolog.
%
%	Incidentatlly, be aware that sometimes the Graphviz engine seems
%	to get stuck if it encounters double quotes (possibly even
%	escaped ones) at which point the way out is to restart the
%	application.
%
sanitise_dot_language_term('"', double_quote).
sanitise_dot_language_term('\'', single_quote).
sanitise_dot_language_term(T, T).
