:-module(production_induction, [given_productions/1
			       ,known_productions/1
			       ,retract_given_productions/0
			       ,retract_derived_productions/0
			       ,listing_grammar_knowledge/0
			       ,compress_corpus/1
			       ,complete_grammar/0
			       ,complete_grammar/1
			       ,examples_corpus/1
			       ,pruned_corpus/3
			       ,derivation/3
			       ,grammar/4
			       ,updated_grammar/3
			       ,production_structure/4
			       ,production_constituents/4
			       ,best_scored_production/4
			       ,production_score/3
			       ,augmentation_set/3
			       ,augmented_production/3
			       ,production//1
			       ]).

:-use_module(project_root(utilities)).
:-use_module(configuration).
:-use_module(library(ordsets)).


%!	given_production(?Name,?Production,?Arguments) is  det.
%
%	Dynamic term used to keep track of given productions.
:- dynamic
	given_production/2.

%!	derived_production(?Name,?Production,?Arguments) is  det.
%
%	Dynamic term used to keep track of derived productions.
:- dynamic
	derived_production/2.

%!	unpruned_corpus_length(?Length) is det.
%
%	The starting length of the unpruned corpus. Used to calculate
%	production scores.
:- dynamic unpruned_corpus_length/1.



%!	given_productions(-Productions) is det.
%
%	A list of all known productions in a grammar of the target
%	language.
%
%	Called by assert_given_productions/0 so asserted here to allow
%	use as directive.
%
given_productions(Ps):-
	% If we're using previously derived productions, get them
	% from nonterminal//0 clauses and treat them as given.
	configuration:dogfooding(true)
	,!
	,known_productions(Ps)
	,debug(query_database, '~w ~w', ['Found given productions (df): ',Ps]).

given_productions(Ps):-
	% If we're not using previously derived productions, treat only the
	% clauses of nonterminal//0 actually declared in the source file
	% as given.
	configuration:dogfooding(false)
	,!
	,findall(Production
		 ,source_file_of(_, Production, _, _)
	       ,Ps)
	,debug(query_database, '~w ~w', ['Found given productions: ',Ps]).

% Dangerous, rather.
given_productions([]):-
	debug(query_database, '~w', ['Found no given productions.']).


%!	known_productions(?Productions) is det.
%
%	Convenience predicate to collect the set of known productions
%	attached to nonterminal//0 terms and asserted in the dynamic
%	database, regardless of whether they were loaded from a source
%	file, learned in a previous run and preserved with the
%	dogfooding option set to true, or indeed derived in the latest
%	run.
%
known_productions(Ps):-
	configuration:rule_complexity(C)
	,configuration:language_module(M)
	% Note this will only get terms with arity //0.
	,findall(P
		,(phrase(M:nonterminal, [N])
		 ,functor(T,N,C)
		 ,clause(M:T,B)
		 ,once(prolog_dcg(T:-B, P)))
	       ,Ps).


%!	source_file_of(?Name,?Production,?Clause_index,?File) is det.
%
%	True when Production is a production declared as a given
%	production (in a given_production/2 term) and also connected to
%	nonterminal//0 and actually declared in the source file of the
%	currently configured language module.
%
%	File is the full path of the source file and Clause_index the
%	index of the nonterminal//0 clause that production is connected
%	to.
%
%	This predicate is meant to help in identifying hand-crafted
%	productions from productions derived in a given run and declared
%	as "given" in subsequent runs (with dogfooding option set to
%	true).
%
%	@TODO: Remove the vestigial third argument (Clause_index).
%
source_file_of(Name, (Name --> B), _Clause_index, language(Source_file_name)):-
	configuration:language_module(M)
	,configuration:output_type(T)
	,configuration:output_format(T,Ext)
	,atom_concat(M, Ext, Source_file_name)
	,read_file_to_terms(language(Source_file_name), Terms, [])
	,member(M:nonterminal --> [Name], Terms)
	,member((Name --> B), Terms).



%!	retract_given_productions is det.
%
%	Clear all given_production/2 terms from the database.
%	Declared here to make it available to
%	assert_given_productions/0.
retract_given_productions:-
	forall(given_production(N,P)
	       ,(retract(given_production(N,P))
		,debug(clear_database, '~w ~w', ['Retracted',given_production(N,P)])
	       )
	      )
	,debug(clear_database, '~w', ['Retracted all clauses of given_production/2']).



%!	assert_given_productions is det.
%
%	Populate the database with clauses of given_production(N,P)
%	one for each given production in the currently loaded language
%	module, where N is the name of the production P.
%
%	This predicate is called at startup to initialise the database
%	with what is essentially background knowledge about the target
%	langugae, encoded as production rules in a known grammar for
%	that language.
%
assert_given_productions:-
	retract_given_productions % cleanup first.
	,given_productions(Ps)
	,forall(member((N --> B), Ps),
		(   assert(given_production(N, (N --> B)) )
		   ,debug(write_to_database, '~w ~w', ['Asserted',given_production(N, (N --> B))])
		)
	       ).



%!	retract_derived_productions is det.
%
%	Clear the database of derived production information. This
%	consists of:
%
%	a) All clauses of derived_production/2 asserted into this
%	   module.
%	b) All clauses of nonterminal//0 referencing the Name of a
%	  derived production (the first argument of a
%	  derived_production/2 clause).
%	c) All Prolog rules created for such productions and asserted
%	   into the language module.
%
%	The above needs to happen in the reverse order than listed
%	above: to get the names of Prolog rules and nonterminals
%	asserted into the language module, we need the corresponding
%	derived_production/2 terms; so these go last. nonterminal//0
%	clauses and clauses of newly asserted rules can go in any order
%	before that.
%
retract_derived_productions:-
	% If we're feeding the algorithm its output we want
	% to keep rules and references learned in the last run
	% in the database for subsequent runs. I fnot, retract them.
	(   dogfooding(false)
	->  retract_derived_productions(rules)
	   ,retract_derived_productions(references)
	;   true
	)
	,retract_derived_productions(clauses).


%!	retract_derived_productions(+Type) is nondet.
%
%	Business end of retract_derived_productions/0. Each clause takes
%	care of a different set of clauses to remove, according to the
%	order documented in the PLDoc comments for the parent predicate.
%
%	Type is one of:
%	* rules, remove clauses of new nonterminals asserted in the
%	language module
%	* references, remove clauses of nonterminal//0
%       * clauses, remove clauses of derived_production/2
%
retract_derived_productions(rules):-
	configuration:language_module(M)
	,forall(derived_production(N, (N --> Ts)),
		(   dcg_translate_rule((N --> Ts), H:-B)
		   ,(   clause(M:H, B, Ref)
		   ->	erase(Ref)
		       ,debug(clear_database, '~w ~w', ['Retracted rule',(H:-B)])
		    ;	true
		    )
		)
	       ).

retract_derived_productions(references):-
	%derived_grammar(G); G = nonterminal. Would be nice to have!
	configuration:language_module(M)
	,forall(derived_production(N, _P),
		(   dcg_translate_rule(nonterminal --> [N], H:-B)
		    ,copy_term(H:-B, H_:-B_)
		   ,(   clause(M:H_, B_, Ref)
		       ,(H:-B) =@= (H_:-B_) % Avoid confusion with cyclic terms.
		   ->	erase(Ref)
		       ,debug(clear_database, '~w ~w', ['Retracted nonterminal',nonterminal --> [N]])
		    ;	true
		    )
		)
	       ).

retract_derived_productions(clauses):-
	forall(derived_production(N,P)
	      ,(retract(derived_production(N,P))
	       ,debug(clear_database, '~w ~w', ['Retracted',derived_production(N,P)])
	       )
	      )
	,debug(clear_database, '~w', ['Retracted all clauses of derived_production/2']).



%!	retract_unpruned_corpus_length is det.
%
%	Clear all unpruned_corpus_length/1 clauses from the database.
%
retract_unpruned_corpus_length:-
	retractall(unpruned_corpus_length(_))
	,debug(clear_database, '~w', ['Retracted all clauses of unpruned_corpus_length/1']).



%!	assert_unpruned_corpus_length is det.
%
%	Set the length of the unpruned corpus according to the examples
%	in the examples module.
%
%	@TODO: think whether can be compiled into a static term for
%	optimisation.
%
assert_unpruned_corpus_length:-
	retract_unpruned_corpus_length
	,findall(Example, configuration:example_string(Example), Examples)
	,length(Examples, L)
	,assert(unpruned_corpus_length(L))
	,debug(write_to_database, '~w ~w', ['Asserted',unpruned_corpus_length(L)]).



%!	listing_background_knowledge is det.
%
%	Print information about the given and derived grammar:
%	* Clauses of nonterminal//0 and terminal//0
%	* The corresponding Prolog rules.
%	* Clauses of given_production/2.
%
%	@Bug: not all terminals have explicitly stated rules with their
%	name- ie, there isn't necessarily a preterminal for each
%	terminal. So the listing will fail with an error.
%
listing_grammar_knowledge:-
	listing(given_production)
	,configuration:language_module(M)
	,listing(M:nonterminal//0)
	,forall(phrase(nonterminal, [N]),listing(M:N))
	,listing(M:terminal//0)
	,forall(phrase(terminal, [N]),listing(M:N)).

% Cleanaup and then prime database
%:-retract_derived_productions.
%:-assert_given_productions.
%:-assert_unpruned_corpus_length.


%!	initialisation(Grammar,Corpus) is det.
%
%	Initialise the Grammar and examples Corpus.
%
%	Grammar is a list:
%       [S, N, T, P] where:
%	  S, the start symbol of Grammar
%         N, the set of nonterminal symbols in Grammar
%         T, the set of terminals in Grammar
%         P, the set of productions in Grammar
%
%	Corpus is the set of example strings in the target language,
%	collected from the currently configured examples_language.
%
initialisation([S,Ns,Ts,Ps],Cs):-
	% Cleanup first.
	debug(init,'~w',['Retracting derived productions.'])
	,retract_derived_productions
	% This should _probably_ be happening.
	,debug(init,'~w',['Asserting given productions.'])
	,assert_given_productions
	,debug(init,'~w',['Asserting unpruned corpus length.'])
	,assert_unpruned_corpus_length
	,debug(init,'~w',['Compiling known grammar.'])
	,grammar(S,Ns,Ts,Ps)
	,debug(init,'~w',['Assembling examples corpus.'])
	,examples_corpus(Cs).



%!	compress_corpus is det.
%
%	Compress the examples corpus using the set of productions in the
%	currently known grammar and write the result to a new examples
%	file. The compressed corpus is then ready to be used to a new
%	run with compression_level set to "second_order".
%
compress_corpus([St,Ns,Ts,Ps]):-
	configuration:compression_level(first_order)
	% Ain't getting the right grammar here.
	% Rather a bug don't you think dear?
	%grammar(St,Ns,Ts,Ps)
	%,!
	,configuration:compression_grammar_output_stream(O)
	,expand_file_search_path(O,P)
	,open(P, write, S, [])
	,print_grammar(S, [St,Ns,Ts,Ps], compression_grammar)
	,close(S)
	,ensure_loaded(O)
	,edit(P)

	,writeln('Compressed corpus')

	,examples_corpus(Cs)
	,configuration:compressed_corpus_output_stream(C_O)
	,expand_file_search_path(C_O,C_P)
	,open(C_P, write, C_S, [])
	,print_compressed_corpus(C_S, compression_grammar, Cs)
	,close(C_S)
	,edit(C_P)

	,writeln('Printed compressed corpus')
	.
compress_corpus(_).

print_compressed_corpus(Stream, Module, Corpus):-
	stream_property(Stream, file_name(Path))
	,file_base_name(Path, Filename)
	,file_name_extension(Corpus_module_name, _Extension, Filename)
	,format(Stream, '~w~w~w~w~w~n'
	       ,[':-module(',Corpus_module_name,','
		,[example_string/1],').'])
	,write(Stream, '\n')
	,forall(member(C, Corpus)
	       ,( phrase(Module:compression_grammar(Comp), C)
		 ,write_term(Stream
			   ,example_string(Comp)
			   ,[fullstop(true)
			    ,nl(true)
			    ,spacing(next_argument)
			    ,quoted(true)
			    ]
			   )
		)
	       )
	.


%!	complete_grammar is det.
%
%	Run that_algorithm and print the results to the configured
%	output stream.
%
complete_grammar:-
	% TODO: remove this from my sight.
	make
	,complete_grammar(G)
	,configuration:output_stream(O)
	,expand_file_search_path(O, P)
	,open(P,write,S,[])
	,configuration:output_type(T)
	,once(print_grammar(S, G, T))
	,close(S)
	,edit(P)
	,(   configuration:output_type(higher_order_grammar)
	->   compress_corpus(G)
	 ;   true
	).


%!	print_grammar(+Stream,+Grammar) is semidet.
%
%	Write the elements of Grammar to Stream, each on a different
%	line: the Start symbol, list of Nonterminals, list of Terminals
%	and each Production.
%
print_grammar(Stream,[S,Ns,Ts,Ps], loose):-
	writeln(Stream, S)
	,writeln(Stream, Ns)
	,writeln(Stream, Ts)
	,forall(member(P,Ps),writeln(Stream,P)).

print_grammar(Stream,[S,Ns,Ts,Ps], terms):-
	% Write the start symbol
	print_term(Stream,s,S)
	% Write the list of nonterminals
	,print_term(Stream, n, Ns)
	% Write the list of terminals
        ,print_term(Stream,t,Ts)
	,forall(member(N, Ns), print_term(Stream, p, nonterminal --> [N]))
	% Write each production on a separate line
	,forall(member(P,Ps),(print_term(Stream,p,P))).

print_grammar(Stream, [S,Ns,Ts,Ps], grammar):-
	configuration:language_module(M)
	,stream_property(Stream, file_name(Path))
	,file_base_name(Path, Filename)
	,file_name_extension(New_module_name, _Extension, Filename)
	,language_module_exports(M,Es)
	% Print module name, exports list and use_module statements.
	,format(Stream, '~w~w~w~w~w~n', [':-module(',New_module_name,',',[S//0|Es],').'])
	% Print the start symbol rule
	,write(Stream, '\n')
	,print_term(Stream, p, (start --> [S]))
	% Print each terminal rule
	,write(Stream, '\n')
	,forall(member(T, Ts), print_term(Stream, p, terminal --> [[T]]))
	% Print each nonterminal rule
	,write(Stream, '\n')
	,forall(member(N, Ns), print_term(Stream, p, nonterminal --> [N]))
	% Connect each production to the start symbol
	,write(Stream, '\n')
	,forall(member(Name-->_Body, Ps),(print_term(Stream,p,S --> Name)))
	% Print each production
	,write(Stream, '\n')
	,forall(member(P,Ps),(print_term(Stream,p,P))).

print_grammar(Stream, [_S,_Ns,_Ts,Ps], higher_order_grammar):-
	configuration:compression_level(first_order)
	,stream_property(Stream, file_name(Path))
	,file_base_name(Path, Filename)
	,file_name_extension(New_module_name, _Extension, Filename)
	,grammar_module_exports(Ps,[],Es)
	% Print module name, exports list and use_module statements.
	,write_term(Stream, (:-module(New_module_name,Es))
		   ,[fullstop(true)
		    ,nl(true)
		    ,spacing(next_argument)
		    ,quoted(true)]
		   )
	% Print each production
	,write(Stream, '\n')
	,forall(member(P,Ps),(print_term(Stream,p,P))).

/*
Still need to print this:

For example, higher order:
destroy_target_t_artifact--> destroy_target, t_artifact.

ie, no brackets.
*/
print_grammar(Stream, [S,Ns,Ts,Ps], higher_order_grammar):-
	configuration:language_module(M)
	,configuration:compression_level(second_order)
	,stream_property(Stream, file_name(Path))
	,file_base_name(Path, Filename)
	,file_name_extension(New_module_name, _Extension, Filename)
	,grammar_module_exports([S-->[]|Ps],[],G_Es)
	,language_module_exports(M,L_Es)
	,append(L_Es, G_Es, Exports)
	% Print module name, exports list and use_module statements.
	,write_term(Stream, (:-module(New_module_name,[first_order_phrase/3,second_order_phrase/2|Exports])
			    )
		   ,[fullstop(true),nl(true),spacing(next_argument),quoted(true)]
		   )
	% Print multifile declaration of language module exports
	,write(Stream, '\n')
	,write_term(Stream, (:-multifile L_Es),
		    [fullstop(true),nl(true),spacing(next_argument)] )
	% Print use_module/1 directive.
	,higher_order_grammar_filename(first_order, FOG_module)
	,write_term(Stream, (:-use_module(FOG_module)),
		    [fullstop(true),nl(true),spacing(next_argument)] )
	% Print first_order_phrase/3 term.
	,write(Stream, '\n')
	,write_term(Stream,
		    (first_order_phrase(Rule, Derivation, Rest_of_sentence):-
		      phrase(nonterminal, [Rule])
		     ,atom(Rule)
		    ,phrase(Rule, Derivation, Rest_of_sentence)
		    )
		   ,[fullstop(true)
		    ,nl(true)
		    ,spacing(next_argument)
		    ,variable_names(['Rule'=Rule
				    ,'Derivation'=Derivation
				    ,'Rest_of_sentence'=Rest_of_sentence])
		    ])
	% Print second_order_phrase/3 term used to parse using second order grammar.
	,write(Stream, '\n')
	,write_term(Stream,
		    (second_order_phrase(Rule, Derivation):-
		      phrase(nonterminal, [Rule])
		     ,atom(Rule)
		     ,phrase(Rule, Second_order_rules)
		     ,findall(D,
			     (member(Second_order_rule, Second_order_rules),
			      phrase(Second_order_rule, D)),
			     Derivation)
		    )
		   ,[fullstop(true)
		    ,nl(true)
		    ,spacing(next_argument)
		    ,variable_names(['Rule'=Rule
				    ,'Derivation'=Derivation
				    ,'Second_order_rule'=Second_order_rule
				    ,'Second_order_rules'=Second_order_rules
				    ,'D'=D])]
		   )
	% Print the start symbol rule
	,write(Stream, '\n')
	,print_term(Stream, p, (start --> [S]))
	% Print each terminal rule
	,write(Stream, '\n')
	,forall(member(T, Ts),print_term(Stream, p, terminal --> [[T]]))
	% Print each nonterminal rule
	,write(Stream, '\n')
	,forall(member(N, Ns),print_term(Stream, p, nonterminal --> [N]))
	% Connect each production to the start symbol
	,write(Stream, '\n')
	,forall(member(Name-->_Body,Ps),(print_term(Stream,p,S -->Name)))
	% Print each production
	,write(Stream, '\n')
	,forall(member(P,Ps),(print_term(Stream,p,P))).

print_grammar(Stream, [_S,_Ns,Ts,Ps], compression_grammar):-
	%configuration:compression_level(first_order)
	%,writeln(you_are_here)
	stream_property(Stream, file_name(Path))
	,file_base_name(Path, Filename)
	,file_name_extension(New_module_name, _Extension, Filename)
	% Print module name, exports list and use_module statements.
	,write_term(Stream, (:-module(New_module_name,[compression_grammar//1]))
		   ,[fullstop(true)
		    ,nl(true)
		    ,spacing(next_argument)
		    ,quoted(true)]
		   )
	% Print compression_grammar//1 term; easiest thing to do to
	% resolve scope of terminal//1 and nonterminal//1 is to print it
	% In the same file as the nonterminal//1 and terminal//1 terms.
	,write(Stream, '\n')
	,write_term(Stream, compression_grammar([]) --> []
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
		   )
	,write_term(Stream,
		    (compression_grammar([A|As])-->terminal(A),!,compression_grammar(As))
		   ,[fullstop(true)
		    ,nl(true)
		    ,spacing(next_argument)
		    ,quoted(true)
		    ,variable_names(['A'=A
				    ,'As'=As])
		    ]
		   )
	% Print each production as a nonterminal//1 clause with the
	% name of the production as the argument.
	,write(Stream, '\n')
	,forall(member(P,Ps)
	       ,(compression_nonterminal(P, Term)
		    ,write_term(Stream, Term
			   ,[fullstop(true)
			    ,nl(true)
			    ,spacing(next_argument)
			    ,quoted(true)]
			   )
		)
	       )
	% Print the single necessary terminal//1 clause.
	,write(Stream, '\n')
	,(write_term(Stream,
		     (terminal(T)-->{phrase(terminal,T)},T)
		    ,[fullstop(true)
		     ,nl(true)
		     ,spacing(next_argument)
		     ,quoted(true)
		     ,variable_names(['T'=T])
		     ]
		    )
	 )
	% Print each terminal as a terminal//0 clause.
	,write(Stream, '\n')
	,forall(member(T ,Ts)
	       ,(write_term(Stream, terminal --> [T]
			   ,[fullstop(true)
			    ,nl(true)
			    ,spacing(next_argument)
			    ,quoted(true)]
			   )
		)
	       ).


%!	compression_nonterminal(+Production,-Nonterminal) is det.
%
%	Ensures nonterminals in the body of a production are properly
%	wrapped in nonterminal//1 terms for use in compression grammar.
%
compression_nonterminal(N --> [T], nonterminal(N) --> [T]).
compression_nonterminal(N --> B, nonterminal(N) --> B):-
	% The body is a list of terminals.
	is_list(B).
compression_nonterminal(N --> B, nonterminal(N) --> nonterminal(B)):-
	% The body is a single nonterminal reference
	atomic(B).
compression_nonterminal(Name --> B, (nonterminal(Name) --> Ts_tree)):-
	% The body is a sequence of nonterminal references
	tree_list(B, Tokens_list)
	% Wrap the nonterminals to nonterminal//1 terms
	,findall(Token
		,(member(T, Tokens_list)
		 ,(is_list(T)
		  ->  Token = T
		  ;   atomic(T)
		  ->  Token = nonterminal(T)
		  )
		 )
		,Ts_list)
	,list_tree(Ts_list, Ts_tree).


%!	print_term(+Stream,+Term_type,+Term) is det.
%
%	Write Term to Stream. Convenience predicate to reduce
%	boilerplate in print_grammar/3.
print_term(S, p, T):-
	write_term(S, T,[fullstop(true),nl(true),spacing(next_argument),quoted(true)]).
print_term(S, Gt, Ls):-
	configuration:grammar_term(Gt, Grammar_term)
	,Term =.. [Grammar_term,Ls]
	,write_term(S, Term,[fullstop(true),nl(true),spacing(next_argument),quoted(true)]).


%!	language_module_exports(-Exports) is det.
%
%	Get the list of predicates exported by the currently configured
%	language module.
%
%	Exports is a list of predicate indicators _in DCG form_. The
%	names of those predicates are taken from currently configured
%	langauge module using module_property/2.
%
%	Because that predicate binds a list of predicate indicators as
%	N/A terms _only_ and we want to list exports as N//A it's not er
%	particularly convenient to separate DCGs from non-DCGs in the
%	language module.
%
%	Instead _every_ exported N/A predicate indicator is converted to
%	a predicate indicator N//A' where A' is equal to A - 2.
%
%	This means that if there are any non-DCG terms in the configured
%	langauge module, they will be added to the new module in DCG
%	form and if your program does not anticipate this, there will be
%	Ogres.
%
%	You won't be anticipating it though because this is called by
%	print_grammar/3 (the dogfooding clause) and that expects to be
%	printing out a list of DCGs exported by the current output
%	module.
%
%	So yeah. If theres' non-DCGs in that module? Ooh boy.
%	Serious ogres. Keep in mind.
%
language_module_exports(Module, Exports):-
	module_property(Module, exports(Es))
	,findall(P//A_DCG
		,(member(P/A, Es)
		 ,A_DCG is A - 2
		 )
		,Exports).


%!	grammar_module_exports(+Productions,+Temp,-Acc) is det.
%
%	Converts between a list of productions and a list of module
%	exports, for example [(p --> [t_1],[t_2])] is converted to
%	[p//0].
%
grammar_module_exports([], Es, Es).
grammar_module_exports([P --> _B|Ps], Temp, Acc):-
       configuration:rule_complexity(C)
       ,Rule_arity is C - 2 % The two vars added by DCG compiler
	,grammar_module_exports(Ps, [P//Rule_arity|Temp], Acc).




%!	complete_grammar(-Grammar) is det.
%
%	Run an iteration of that_algorithm and report the completed
%	grammar.
%
complete_grammar(Complete_grammar):-
	timestamp(Start_time)
	,debug(main_loop, '~w ~w', ['Starting new run on',Start_time])
	,configuration_logging
	,initialisation(G,Cs)
	,complete_grammar(G, Cs, Cs, Complete_grammar)
	,timestamp(End_time)
	,debug(main_loop, '~w ~w', ['Ended run on',End_time])
	,! % Red- need to understand where choicepoints are created
	   % and which ones can be nipped in the bud.
	.



%!	configuration_logging is det.
%
%	Convenience predicate to log a list of configuration
%	options declared in configuration module. Used by
%	complete_grammar/1 to log the context of the current run.
%
configuration_logging:-
	debug(main_loop, '~w ', ['Configured options for this run:'])
	,rule_complexity(C)
	,debug(main_loop, '~w ~w', ['Rule complexity:',C])
	,initial_score(Score)
	,debug(main_loop, '~w ~w', ['Initial score:',Score])
	,output_stream(output(F))
	,debug(main_loop, '~w ~w', ['Output stream:',F])
	,output_type(T)
	,debug(main_loop, '~w ~w', ['Output type:',T])
	,production_scoring_strategy(Strategy)
	,debug(main_loop, '~w ~w', ['Production scoring strategy:',Strategy])
	,dogfooding(D)
	,debug(main_loop, '~w ~w', ['Dogfooding:',D]).



%!	complete_grammar(+Grammar,+Corpus,+Corpus_copy,-Updated_gramamr) is det.
%
%	Business end of complete_grammar/1.
%
%	@TODO: Corpus_copy is um... redundant? I'm not sure entirely-
%	give it a look and fix if needed. I think it was there
%	originally to carry around the unpruned corpus when I was using
%	it in deriving a new production; I'm now using the pruned corpus
%	I think.
%
complete_grammar(G, [], _, G). %Exit with a new grammar
	%  For each example in the examples corpus
complete_grammar(G, [C|_Xs], Cs, Acc):-
	debug(next_example, '~w ~w ~w~w ~w', ['Selected',new,example,:,C])
	% Add the example's tokens to the set of terminals
	,new_terminals(C)
	,debug(update_grammar,'~w', ['Added example tokens to the set of Terminals'])
	%  Create a new, originally empty production
	%  [Update] Build the set of augmentation terms
	,augmentation_set(C, G, As)
	,! % Red hot cut- document.
	,debug(update_augmentation_set,'~w ~w', ['Built new augmentation set',As])
	%  For each term in the set of augmentation terms
	% [Build up a new production]
	,derived_production(As, Cs, ypsilon, P)
	,debug(new_production,'~w ~w',['Derived',P])
	%  Add the new production to the grammar
	,updated_grammar(P,G,G_)
	,debug(update_grammar,'~w ~w', ['Updated grammar:',G_])
	%Prune the corpus
	,pruned_corpus(Cs,G_,Cs_)
	%Repeat while there are more examples [in the _un_ pruned corpus]
	,complete_grammar(G_,Cs_,Cs_,Acc).


%!	new_terminals(+Example) is det.
%
%	Add each terminal
%
%	TODO: the bit where we assert a new production to the language
%	module is done so often that it bears making into a utility
%	predicate. Make it so.
%
new_terminals(Example):-
	configuration:language_module(M)
	,forall(member(Token, Example),
		(   dcg_translate_rule(terminal --> [[Token]], H:-B)
		    ,copy_term(H:-B, H_:-B_) % Prooobably not needed - should remove for speed.
		   ,(   \+ clause(M:H_, B_) ,(H:-B) =@= (H_:-B_) % Avoid confusion with cyclic terms.
		   ->	asserta( M:H:-B )
		       ,debug(write_to_database,'~w ~w ~w ~w', ['Asserted',terminal --> [[Token]],'into language module',M])
		    ;	true
		    )
		)
	       ).


%!	derived_production(+Augmentation_set,+Corpus,+Temp,-Acc) is nondet.
%
%	Create a new production from the terms in the Augmentation_set,
%	score it against the Corpus and bind the result to the
%	Accumulator.
%
derived_production([], _Cs, P, P_):-
	named_production(P, P_)
	,!.
	%    Take a new term from the set of augmentation terms
derived_production([A|As], Cs, P, Acc):-
	%    Augment the current production using the new term
	once(augmented_production(P,A,P_))
	,debug(augment_production,'~w ~w ~w ~w',['Augmented',P,to,P_])
	%    Score the production
	%    If the score is 0, discard this version of the production
	%    Otherwise, keep the newest, best scored version of the production
	,once(best_scored_production(Cs, P, P_, P_best))
	%  Repeat while there are more terms [in the augset]
	,derived_production(As,Cs,P_best,Acc).



%!	named_production(+Production,-Renamed) is det.
%
%	Assign a meaningful name to a newly derived production that is a
%	concatenation of the atomic forms of the production's right-hand
%	side elements.
%
%	@TODO: fix the hardcoding.
%
named_production(_ --> [Body], New_name --> [Body]):-
	% Grrr- don't hard-code these!! Daaah.
	atomic_concat(t_, Body, New_name)
	,!.
% Not sure if this is possible anymore.
named_production(_ --> Body, New_name --> Body):-
	atomic(Body)
	,atomic_concat(n_, Body, New_name)
	,!.
named_production(_ --> Body, New_name --> Body):-
	tree_list(Body, Tokens_list)
	,tokens_atomic(Tokens_list, [], Atomic_tokens)
	,reverse(Atomic_tokens, [H|Right_way_through])
	,(   number(H)
	->   atomic_list_concat([num,H|Right_way_through], '_', New_name)
	 ;   atomic_list_concat([H|Right_way_through], '_', New_name)
	).


%!	tokens_atomic(+Tokens,+Temp,-Acc) is det.
%
%	Atomises a list of tokens that may contain compounds, including
%	nested lists and numbers (numbers can't be the first character
%	of a predicate functor).
%
%	Clarifying, the output is a list of atomic tokens, not an atom.
%	The list is passed to named_production/2 that does the atomic
%	concatentation.
%
%	@TODO: if the first element of the output list is a number we
%	can deal with it in named_production/2 without having to check
%	every character here.
%
tokens_atomic([], [[]|Ls], Ls):- !.
tokens_atomic([], Ls, Ls):- !.
tokens_atomic([N|Ns], Temp, Acc):-
	compound(N)
	,compound_name_arguments(N, _, Compound_args)
	,tokens_atomic(Compound_args, [], List_atoms)
	,append(List_atoms, Temp, New_temp)
	,tokens_atomic(Ns, New_temp, Acc).
tokens_atomic([N|Ns], Temp, Acc):-
	tokens_atomic(Ns, [N|Temp],Acc).



%!	examples_corpus(+Examples) is det.
%
%	All examples in the examples corpus.
examples_corpus(Examples):-
	findall(Example,configuration:example_string(Example),Examples).


%!	pruned_corpus(+Corpus,+Grammar,-Pruned_corpus) is det.
%
%	Prune the Corpus using productions in the current Grammar,
%	binding the result in Pruned_corpus.
%
%	Each example in the Corpus is parsed by each production in
%	Grammar, then replaced with the tokens left unparsed by that
%	production.
%
%	Empty examples (ie ones fully parsed) are removed altogether.
%
%	@TODO: I wonder if I should be parsing with the complete
%	grammar, from the start symbol onwards; would that give me a
%	more hierarchical grammar like I want it?
%
pruned_corpus(Corpus, [_S,_Ns,_Ts,Ps], Pruned):-
	debug(prune_corpus,'~w ~w',['Pruning corpus:',Corpus])
	,pruned_corpus_(Ps,Corpus,Pruned)
	,debug(prune_corpus,'~w ~w',['Pruned corpus:',Pruned]).


%!	pruned_corpus_(+Productions,+Corpus,-Pruned_corpus) is det.
%
%	Business end of pruned_corpus/3. Prune Corpus using eacy
%	Production and bind the result to Pruned_corpus.
%
%	@TODO: As an optimisation, if Corpus = [], there's no need to
%	keep going and trying to prune with more rules; so add a second
%	boundary condition: pruned_corpus(_, [], []).
%
pruned_corpus_([],Pruned,Pruned).
pruned_corpus_([P|Ps],Corpus,Acc):-
	debug(prune_corpus,'~w ~w',['Pruning corpus using',P])
	,production_pruned_corpus(Corpus, P, Pruned_corpus)
	,debug(prune_corpus,'~w ~w ~w ~w',['Pruned with',P,to,Pruned_corpus])
	,pruned_corpus_(Ps,Pruned_corpus,Acc).


%!	production_pruned_corpus(+Corpus,+Production,-Pruned_corpus) is det.
%
%	Prune the Corpus using Production and bind the result to
%	Pruned_corpus.
%
%	@TODO: switch Corpus/Production around to match naming.
%
%	About the red cut: the head of the second and third clause of
%	production_pruned_corpus/5 are identical:
%         production_pruned_corpus([C|Cs], M, R, Temp, Acc)
%
%	Which means there's always a choicepoint created when we enter
%	the second clause, regardless of whether the call to
%	derivation/3 in the body of the second clause fails (which is
%	the intended criterion for trying the third clause or not).
%
%	So there's a lot of backtracking over calls to the second
%	clause. The cut at the end of production_pruned_corpus/3 stops
%	this, because the backtracking is unproductive (since we want to
%	try the third clause only if derivation/3 fails).
%
%	The cut is red because it's difficult to follow the behaviour,
%	and also because it's at the end of the only clause of
%	production_pruned_corpus/3 (rather than /5) not because it
%	changes the behaviour of the predicate significantly: the
%	results of backtracking over the third clause are not valid
%	prunes of the corpus- they are partial prunes that we don't care
%	about.
%
%	It's also worth noting that the cut could be placed right after
%	the head of production_pruned_corpus/5 with the same results -
%	but that cuts results we need when calling this predicate from
%	pruned_corpus/3.
%
production_pruned_corpus(Corpus, Production, Pruned_corpus):-
	configuration:language_module(M)
	% Hacky - too hard to mix in Score with derivation/3
	% We don't need it beyond this point anyway so ditch.
	,production_rule(Production, Rule)
	,production_pruned_corpus(Corpus, M, Rule, [], Pruned_corpus)
	,!. % Red cut- see comments.


%!	production_pruned_corpus(+Corpus,+Language_module,+Production,+Temp ,-Acc) is det.
%
%	Business end of production_pruned_corpus/3.
%
production_pruned_corpus([], _, _, Denurp, Pruned):-
	reverse(Denurp, Pruned).
production_pruned_corpus([C|Cs], M, R, Temp, Acc):-
	derivation(M:R, C, Rest)
	,debug(prune_corpus,'~w ~w ~w ~w ~w ~w',['Used rule',R,'to prune',C,to,Rest])
	,optimisation(fully_consumed_example, [Rest, Temp], Updated)
	,production_pruned_corpus(Cs, M, R, Updated, Acc).
production_pruned_corpus([C|Cs], M, R, Temp, Acc):-
	debug(prune_corpus,'~w ~w ~w',[R,'pruned 0 tokens from',C])
	,production_pruned_corpus(Cs, M, R, [C|Temp], Acc).



%!	derivation(+Production,+Derivation,-Rest_of_sentence) is det.
%
%	Parse Derivation with Production and bind a reference to the
%	Rest_of_sentence.
%
%	Kind of does what phrase/3 does, but for rules that have not yet
%	been added to the database.
%
%	@TODO: Add lots of tests
%	@TODO: Make sure scores are handled gracefully (not necessarily
%	in this predicate- might need to do that before calling it)
%	@TODO: Use in scoring productions rather than what we do
%	now. @TODO: Move to utilities, possibly.
%
derivation(_M:(H:-true), D, Rest):-
	duplicate_term(H, H_)
	% Probably breaks with rules arity of more than 1
	,H_ =.. [_Name|[Tokens|Rest]]
	,[Tokens|Rest] = D.
derivation(M:(H:-B), D, Rest):-
	duplicate_term(H:-B, H_:-B_)
	,M:B_
	,H_ =.. [_Name|[D,Rest]].



%!	grammar(-Start,-Nonterminals,-Terminals,-Productions) is det.
%
%	A grammar, as a quadruple consisting of a Start symbol, a set of
%	Nonterminals, a set of Terminals and a set of Productions. Each
%	set is ordered according to the standard order of terms and
%	no duplicates are allowed.
%
grammar(Start,Nonterminals,Terminals,Productions):-
	once(phrase(configuration:start, [Start]))
	,grammar_nonterminals(Nonterminals)
	,grammar_terminals(Terminals)
	% Not strictly necessary but, why not? Won't run in critical region, probably.
	% Also, the N^... is needed to avoid backtracking for more- use bagof/3 if
	% refactoring- not findall/3.
	,grammar_productions(Productions)
	,debug(init,'~w ~w ~w ~w ~w ~w ~w ~w',['Found grammar with star tsymbol:',[Start]
			 ,'nonterminals:',Nonterminals
			 ,'terminals:',Terminals
			 ,'productions:',Productions]).


%!	grammar_nonterminals(-Nonterminals) is det.
%
%	The set of all Nonterminals known to the given grammar.
%	Nonterminals can be [] if no Nonterminals are known.
%
%	@TODO: the way this is defined it will succeed also when the
%	nonterminals in the given grammar are somehow declared in a way
%	that makes the call to nonterminal//0 fail, for instance if some
%	nonterminals have double brackets by mistake etc. Might wanna
%	re-think. Same goes for grammar_terminals/1 and
%	grammar_productions/0.
%
grammar_nonterminals(Nonterminals):-
	  setof(N, phrase(configuration:nonterminal, [N]), Nonterminals)
	; Nonterminals = [].


%!	grammar_terminals(-Terminals) is det.
%
%	The set of all Terminals known to the given grammar. Can be [].
%
%	@TODO: think wether this needs to get terminals from
%	derived_production/2 clauses also.
%
grammar_terminals(Terminals):-
	% Note the twice-bracketed terminal:
	  setof(T,phrase(configuration:terminal, [[T]]), Terminals)
	; Terminals = [].


%!	grammar_productions(-Productions) is nondet.
%
%	The set of all Production known to the given grammar. Can be [].
%
%	@BUG: this only gets given productions; if there are any derived
%	shouldn't it also present them, at the very least depending on
%	the value of the dogfooding option?
%
grammar_productions(Productions):-
	  setof(P,N^given_production(N,P),Productions)
	; Productions = [].



%!	grammar_s(Start,Nonterminals,Terminals,Productions) is det.
%
%	Ordered-set and difference-list version of grammar/4, where each
%	of Nonterminals,Terminals and Productions is an ordered set (as
%	in library(ordsets)) with a Tail-variable appended to its end
%	with -/2.
%
%	@TODO: this is not up-to-date with changes to grammar/4,
%	particularly the way we get all terminals, nonterminals and
%	productions using grammar_* predicates.
%
grammar_s(Start,Nonterminals-Ns_t,Terminals-Ts_t,Productions-Ps_t):-
	once(phrase(configuration:start, [Start]))
	,findall(N, phrase(configuration:nonterminal, [N]), Ns)
	,list_to_diff_ordset(Ns, Nonterminals, Ns_t)
	,findall(T,phrase(configuration:terminal, [[T]]), Ts)
	,list_to_diff_ordset(Ts, Terminals, Ts_t)
	,findall(P,given_production(_N,P),Ps)
	% Yes, productions (and generally compound terms) are also sortable.
	,list_to_diff_ordset(Ps, Productions, Ps_t).



%!	updated_grammar(+Production,+Grammar,-Updated_grammar) is det.
%
%	Update the current grammar with a newly learned production.
%
%       @TODO: It would	be very esy here to connect terminals to the
%	terminal//0 term - and in fact do the same for nonterminals. I
%	could even try to lexicalize either.
%
updated_grammar((_ --> []), [S,Ns,Ts,Ps], [S,Ns,Ts,Ps]):-
	debug(update_grammar,'~w', ['Discarded empty production']).
updated_grammar((N --> T), [S,Ns,Ts,Ps], [S,Ns,Ts,Ps]):-
	% New rule for a single nonterminal- discard it.
	atom(T)
	,debug(update_grammar,'~w ~w', ['Discarded single nonterminal:',N --> T]).
updated_grammar(Production, [S,Ns,Ts,Ps], [S,Ns_,Ts_,[(Name --> Tokens)|Ps]]):-
	debug(update_grammar,'~w ~w', ['Updating grammar with:',Production])
	,once(production_structure(Production,Name,_Score,Tokens))
	,tree_list(Tokens, Tokens_list)
	,once(phrase(symbols(nonterminal, P_Ns), Tokens_list, P_Ts))
	% Unbracketing terminals. Don't ask.
	,(   P_Ts \= []
	 ->  [P_Ts_unbracketed] = P_Ts
	 ;   P_Ts_unbracketed = []
	 )
	% The Name of the new production is a nonterminal:
	,list_to_ord_set([Name|P_Ns], Ns_ord)
	,list_to_ord_set(P_Ts_unbracketed, Ts_ord)
	,ord_union(Ns_ord, Ns, Ns_)
	,ord_union(Ts_ord, Ts, Ts_)
	,update_grammar(Name --> Tokens).


%!	update_grammar(+Production) is det.
%
%	Add a new production to the database.
%
%	@TODO: Add more context to this PLdoc.
%	@TODO: is it necessary to add a nonterminal//0 to the language
%	module? We're adding it to the output file anyway...?
%
update_grammar(Name --> Tokens):-
	debug(write_to_database,'~w ~w ~w', ['Adding:',Name --> Tokens,'to database.'])
	,configuration:language_module(M)
	% Remember this derived production until next run
	,asserta(derived_production(Name, (Name --> Tokens)))
	,debug(write_to_database,'~w ~w ~w', ['Asserted',derived_production(Name, (Name --> Tokens)),term])
	% Add to the set of known nonterminals for this run
	,dcg_translate_rule(nonterminal --> [Name], Nonterminal)
	,asserta(M:Nonterminal)
	,debug(write_to_database,'~w ~w ~w ~w', ['Asserted',Nonterminal,'into language module',M])
	% Add to rules for this run
	% Er. Shouldn't I be adding the score also?
	,dcg_translate_rule(Name --> Tokens, Rule)
	,asserta(M:Rule)
	,debug(write_to_database,'~w ~w ~w ~w', ['Asserted rule',Rule,'into language module.',M]).



%!	updated_grammar_s(+Production,+Grammar,-Updated_grammar) is det.
%
%	Ordset and diff-list version of updated_grammar/3, meant to
%	receive input from grammar_s/4 rather than grammar/4.
%
updated_grammar_s((_ --> []), [S,Ns,Ts,Ps], [S,Ns,Ts,Ps]).
updated_grammar_s((_ --> T), [S,Ns,Ts,Ps], [S,Ns,Ts,Ps]):-
	atom(T).
updated_grammar_s((Name --> Tokens), [S,Ns-Ns_t,Ts-Ts_t,Ps-Ps_t], [S,Ns1-Ns_t1,Ts1-Ts_t1,Ps1-Ps_t1]):-
	tree_list(Tokens, Tokens_list)
	,once(phrase(symbols(nonterminal, P_Ns), Tokens_list, P_Ts))
	,diff_append(Ns-Ns_t, P_Ns-[], Ns_bag-[])
	,list_to_diff_ordset(Ns_bag, Ns1, Ns_t1)
	,[Ts_unbracketed] = P_Ts
	,diff_append(Ts-Ts_t, Ts_unbracketed-[], Ts_bag-[])
	,list_to_diff_ordset(Ts_bag, Ts1, Ts_t1)
	,diff_append(Ps-Ps_t,[(Name --> Tokens)|Ps_t1]-Ps_t1,Ps1-Ps_t1).



%!	production_rule(+Production, -Rule) is det.
%
%	Convert between a Production in DCG notation to a Prolog rule.
%
%	If Production is of the form:
%	==
%	Name, Score --> Body
%	==
%
%	Then Score is dropped and the resulting Rule is equivalent to
%	the output of:
%	==
%	dcg_translate_rule(Name --> Body, Rule).
%	==
%
%	Use to avoid having to explicitly unify production terms with
%	dcg parts.
%
production_rule(Production, Rule):-
	production_structure(Production,Name,_,Body)
	,dcg_translate_rule(Name --> Body, Rule).



%!	production_structure(+Production,-Name,-Score,-Body) is semidet.
%
%	True when Production is a term of the form:
%	  Name, Score --> Body.
%
%	Or of the form:
%	  Name --> Body.
%
%	Used to extract the components of a production from a variable
%	bound to the whole production when there's no need for the
%	costlier extraction of constituents.
%
production_structure((Name --> Body),Name,[],Body):-
	atom(Name)
	,!. % Reddish cut- if Name is an atom and Score is [] we should stop here
	   % else we end up with A term (Name, [] --> Body) on backtracking
production_structure((Name, Score --> Body),Name,Score,Body).



%!	production_constituents(?Name,?Production,?Constituents,+Options) is semidet.
%
%	True when Name is the name of a production term in DCG form:
%	==
%	Name,[Score] --> Nonterminals, Terminals.
%	==
%
%	and Constituents is a list of its constituents:
%	==
%	Constituents = [Score,Nonterminals,Terminals]
%	==
%
%	On successive backtracking, production/3 will enumerate all
%	productions in the target grammar, both given and derived.
%
%	Options:
%	  type(T), one of: given, derived.
%
production_constituents(Name, Production, [Score, Nonterminals, Terminals], [type(given)]):-
	production_constituents(given_production, Name, Production, Score, Nonterminals, Terminals).

production_constituents(Name, Production, [Score, Nonterminals, Terminals], [type(derived)]):-
	production_constituents(derived_production, Name, Production, Score, Nonterminals, Terminals).


%!	production_constituents(+Type,?Name,?Production,?Score,?Nonterminals,?Terminals) is nondet.
%
%	Business end of production_constituents/5; takes care of both
%	given and derived production.
%
%	Type is one of: given_production or derived_production.
%
%	@TODO: can probably remove the diff_list/3 calls now that I
%	refactored symbols//2 to get back un-diffed lists for Ns and Ts.
%
production_constituents(Type,Name,(Name, Score --> Tokens), Score, Nonterminals, Terminals):-
	Scored_production =.. [Type,Name,(Name, Score --> Tokens)]
	,Unscored_production =.. [Type,Name,(Name --> Tokens)]
	,(   Scored_production
	;   Unscored_production
	)
	% Assign default score, if the production is not scored.
	% Else leave it as it is. OK, that looks weird but we're just testing
	% that Score is a variable by attempting to bind it to [-1]. The ; true
	% Just means the goal succeeds if the bind fails, because then we have a
	% different binding that we should preserve. Weird, but ISO.
	,(   Score = [-1]
	 ->  true
	 ;   true
	 )
	,tree_list(Tokens, Tokenlist)
	,once(phrase(symbols(nonterminal, Nonterminals), Tokenlist, Terminals)).



%!	empty_production(?Ypsilon) is nondet.
%
%	True when Ypsilon is a term:
%	(N, [R] --> [])
%
%	Representing the empty production, where N is a legal production
%	name and R the initial probability of a rule, as given by
%	initial_score/1.
empty_production((N, [R] --> [])):-
	var(N)
	,configuration:internal_production_name(N)
	,configuration:initial_score(R).

empty_production((N, [R] --> [])):-
	nonvar(N)
	,atomic(N)
	,configuration:initial_score(R).


%!	production_name(+Name) is det.
%
%	True when Name is a valid production name, conforming to the
%	expression:
%	==
%	[a-z][a-zA-Z0-9_]*
%	==
%
production_name(N):-
	must_be(nonvar, N)
	,atom_chars(N, [A|Atomic])
	,char_type(A, lower)
	,forall(member(C, Atomic), (char_type(C, alnum); C = '_')).



%!	best_scored_production(+Corpus,+Production,+Augmented_production,-Best) is det.
%
%	Choose the Best between Production and Augmented_production.
%
%	"Best" is the production with the highest generalisation score
%	ie the one that best genealises over examples of Corpus.
%
best_scored_production(Cs, P, P_, P_best):-
	scored_production(Cs, P_, P_scored)
	,best_scored_production(P, P_scored, P_best)
	,debug(score_production,'~w ~w ~w ~w~w ~w ~w',['Best scored',P_best,'(between:',P,',',P_scored,')'])
	.



%!	best_scored_production(+Production,+Augmented_production,-Best) is det.
%
%	Business end of best_scored_production/4. Selects the Best
%	scored of Production and Augmented_production.
%
%	Augmented_production is a built-up, newly scored version of
%	Production.
%
%	If Production is the empty production and the score of
%	Augmented_production is not 0, Best is bound to
%	Augmented_production.
%
%	Otherwise, if the score of Augmented_production is better than
%	that of Production, then Best is bound to Augmented_production.
%
%	In all other cases Augmented_production is "discarded" and Best
%	is bound to Production.
%
best_scored_production(ypsilon, ypsilon, ypsilon).
best_scored_production(Production, (_N, [0] --> _B), Production).
best_scored_production((_N, [0] --> _B), Augmented, Augmented). % For clarity (same as last clause)
best_scored_production((N, [S] --> B), (N, [S_] --> _), (N, [S] --> B)):-
	S > S_
	,S \= 0
	,!. % Cut to avoid binding again to wrong result in next clause.
best_scored_production(_Production, (N, [S] --> B), (N, [S] --> B)):-
	S \= 0.



%!	scored_production(+Corpus,+Production,-Scored_production) is det.
%
%	If Production is a scored production, of the form:
%	==
%	Name, [Score] --> Body.                      % [1]
%	==
%
%	- this updates the Score to the ratio P:C, where P is the number
%	of examples in Corpus that Production can parse at least
%	partially and C the number of examples in the original, unpruned
%	corpus.
%
%	Otherwise, if Production is an unscored production, of the form:
%	==
%	Name --> Body.
%	==
%
%	- then scored_production/3 calculates its Score as above and
%	adds it as a pushback-list, resulting in a term like the scored
%	production in [1].
%
%	Score evaluates the generalisation power of Production over the
%	given Corpus. It's a Real number from 0 to 1 where a higher
%	value indicates a more general rule.
%
%	A Score of 0 means the rule cannot explain any of the examples
%	in the Corpus and should be discarded, whereas a score of 1
%	means the rule can at least partially explain each example in
%	the Corpus.
%
scored_production(Corpus, Production, (Name, [Score] --> Body)):-
	production_structure(Production, Name, _Score, Body)
	,production_score(Corpus, (Name --> Body), Score).


%!	production_score(+Production,-Updated_production) is det.
%
%	Update the score of the given production to the proportion of
%	examples it can parse at least partially.
%
production_score(Corpus, (Name --> Body), Score):-
	configuration:production_scoring_strategy(S)
	,production_score(S, Corpus, (Name --> Body), Score)
	,debug(score_production, '~w ~w ~w ~w', ['Scored',(Name --> Body),with,Score]).


%!	production_score(+Strategy,+Corpus,+Production,-Score) is nondet.
%
%	Business end of production_score/3. Handles the differente
%	production scoring strategies.
%
%	Strategy is the atomic name of the scoring strategy configured
%	in configuration:production_scoring_strategy/1.
%
production_score(parsed, Corpus, (Name --> Body), Score):-
	configuration:language_module(M)
	,dcg_translate_rule(Name --> Body, R)
	,findall(Example
		,(member(Example, Corpus)
		 ,derivation(M:R, Example, _)
		)
		,Parsed)
	,length(Parsed, Parses)
	,unpruned_corpus_length(Unpruned_length)
	,Score is Parses / Unpruned_length.

production_score(mode, Corpus, (Name --> Body), Score):-
	configuration:language_module(M)
	,dcg_translate_rule(Name --> Body, R)
	,findall(Parsed_proportion
		,(member(Example, Corpus)
		 ,(   derivation(M:R, Example, Unparsed_tokens)
		  ->  length(Example, Example_length)
		     ,length(Unparsed_tokens, Unparsed_length)
		     ,Parsed_length is Example_length - Unparsed_length
		     ,Parsed_proportion is Parsed_length / Example_length
		     ,debug(score_production, '~w ~w ~w ~w ~w ~w ~w ~w ~w ~w'
			   ,['Unparsed tokens:',Unparsed_tokens
			    ,'Example length',Example_length
			    ,'Parsed',Parsed_length
			    ,'Unparsed',Unparsed_length
			    ,'Parsed %',Parsed_proportion
						       ])
		  ;  Parsed_proportion = 0 % Example failed to parse.
		  ,debug(score_production, '~w ~w', [(Name --> Body),'failed to parse example'])
		  )
		)
		,Proportions)
	,sort(Proportions, Sorted)
	,reverse(Sorted, [Score|_]).

production_score(mean, Corpus, (Name --> Body), Score):-
	configuration:language_module(M)
	,dcg_translate_rule(Name --> Body, R)
	,findall(Parsed_proportion
		,(member(Example, Corpus)
		 ,(   derivation(M:R, Example, Unparsed_tokens)
		  ->  length(Example, Example_length)
		     ,length(Unparsed_tokens, Unparsed_length)
		     ,Parsed_length is Example_length - Unparsed_length
		     ,Parsed_proportion is Parsed_length / Example_length
		  ;  Parsed_proportion = 0
		  )
		)
		,Parsed_proportions)
	,foldl(sum_of, Parsed_proportions, 0, Sum_of_proportions)
	,length(Corpus, Corpus_length)
	,Score is Sum_of_proportions / Corpus_length
	,true.

production_score(sum_of_means, Corpus, (Name --> Body), Score):-
	configuration:language_module(M)
	,dcg_translate_rule(Name --> Body, R)
	,findall(Parsed_proportion
		,(member(Example, Corpus)
		 ,(   derivation(M:R, Example, Unparsed_tokens)
		  ->  length(Example, Example_length)
		     ,length(Unparsed_tokens, Unparsed_length)
		     ,Parsed_length is Example_length - Unparsed_length
		     ,Parsed_proportion is Parsed_length / Example_length
		  ;  Parsed_proportion = 0
		  )
		)
		,Parsed_proportions)
	,foldl(sum_of, Parsed_proportions, 0, Sum_of_proportions)
	,unpruned_corpus_length(Unpruned_length)
	,Score is Sum_of_proportions / Unpruned_length
	,true.



%!	augmentation_set(+Example,-Grammar,-Augset) is det.
%
%	Build up the set of augmentation terms, as a list of:
%	[Ns,Ts,Ex]
%
%	Where Ns is nonterminals, Ts nonterminals, from the given
%	grammar and Ex the tokens from a single example.
%
%	@TODO: it's costly having to rebracket terminals in the list of
%	terminals in Grammar; consider leaving them bracketed in the
%	first place, then flattening the list at the end of a run. If
%	that's possible.
%
augmentation_set(Example, [_S,Ns,[],_Ps], Augset):-
	findall([Ex],member(Ex, Example),Bracketed_example)
	,append(Ns, Bracketed_example, Augset).

augmentation_set(Example, [_S,Ns,Ts,_Ps], Augset):-
	setof([Token], member(Token, Example),Example_tokens)
	% Rebracket terminals:
	,(   setof([T], member(T, Ts), Bracketed_Terminals)
	 ;   Bracketed_Terminals = []
	 )
	,ord_subtract(Bracketed_Terminals, Example_tokens, Terminals_minus_Example)
	,findall([Token],member(Token, Example),Bracketed_unordered_example)
	,append(Bracketed_unordered_example, Terminals_minus_Example, Example_and_Terminals)
	,append(Ns, Example_and_Terminals, Augset).



%!	augmented_production(?Production, ?Token, ?Augmented) is nondet.
%
%	True when Production is a grammar rule in DCG notation, and
%	Augmented is the same rule augmented by the given Token.
%
%	Production can be the special atom ypsilon signifying the empty
%	production (as generated by empty_production/2):
%
%	(Name, [-1] --> [])
%
%	Otherwise, Production and Augmented are both in the form:
%
%	==
%	(Name, Score --> Nonterminals, Terminals)
%	==
%
%	Where:
%	* Name is a mix of atoms and numbers, for example as returned by
%	  rule_name/1
%	* Score is a list of integers, representing the probabilities of
%	  Production and each production in a single derivation of
%	  Production.
%	* Nonterminals is zero or more atomic names of nonterminals in
%	  the grammar of the target language, and
%	* Terminals is a list of zero or more atomic nameks of terminals
%	  in the grammar.
%
%	Token is a single terminal or nonterminal, where augmenting
%	Production using Token produces Augmented.
%
%	An atomic token (g1, g2, np, vp...) signifies a nonterminal,
%	whereas a token that is a list with a single element ([a], [b],
%	[abc]...) signifies a terminal.
%
%	augmented_production/3 can be used in mode (+,?,+) to find the
%	difference between two productions.
%
%	... well, it _should_ anyway :D
%
%	TODO: see fourth clause- I removed a whole branch and tests pass
%	anyway. This takes a lot of refactoring still.
%
augmented_production(ypsilon, Token, (Name, Score --> Token)):-
	empty_production((Name, Score --> [])).

augmented_production((Name, _ --> [T|Terminals]), Token, (Name, [S] --> Augmented)):-
	augmented_production([], [T|Terminals], Token, [T|Terminals], Augmented)
	,configuration:initial_score(S).

augmented_production((Name, _ --> Body), Token, (Name, [S] --> Augmented)):-
	tree_list(Body, Tokens)
	,once(phrase(symbols(nonterminal, Nonterminals), Tokens, Terminals))
	,augmented_production(Nonterminals, Terminals, Token, Tokens, Augmented)
	,configuration:initial_score(S).


%!	augmented_production(+Nonterminals,+Terminals,+Token,+Body,-Augmented)	is nondet.
%
%	Business end of augmented_production/3. Handles each separate
%	case of current production - new token in turn.
%
%	In general, there are three possible cases of a current
%	production and its augmentation:
%
%	| Current production |  Augmented production  |
%	| ------------------ |	--------------------  |
%	| []		     |	n \ t                 |
%	| n+		     |	n \ t                 |
%	| n* t+		     |	t                     |
%
%	The empty-production case is handled in the first clause of
%	augmented_production/3.
%
%	The remaining cases are broken down for convenience. In
%	particular, "n* t+ --> t" is handled by two clauses, one for "t
%	--> t", one for "n+ --> t" and one for "n+ t+ --> t".
%
%	As detailed in augmented_production/3 a nonterminal is never
%	allowed to follow a terminal.
%

% Case: n+ --> n | t (one or more terminals augmented by any token).
augmented_production([_|_], [], Token, Body, Augmented):-
	% Call with once/1 to avoid infinitely appending difflist Ns
	once(append(Body, [Token], Tokens))
	,list_tree(Tokens, Augmented).

% Case: t --> t (one terminal, augmented by one terminal)
augmented_production([], [[T]], [Token], _Body, Augmented):-
	once(append([T], [Token], Augmented))
	,!.

augmented_production([], [T|Tokens], [Token], _Body, Augmented):-
	once(append([T|Tokens], [Token], Augmented))
	,!.

% Case: n+ t* --> t (one or more nonterminals followed by one or more
% terminals, augmented by one terminal.
augmented_production([N|Ns], [[T|Ts]], [Token], _Body, Augmented):-
	once(append([T|Ts], [Token], Terminals))
	,once(append([N|Ns], [Terminals], Tokens ))
	,list_tree(Tokens, Augmented).



%!	production// is nondet.
%
%	The structure of a production.
production([H|Ts]) --> production_head(H), production_tokens(Ts).

%!	production_head// is nondet.
%
%	The head of a production consists of a rule name, a score and
%	the dcg arrow, -->.
production_head([N,S,A]) --> rule_name(N), score(S), dcg_arrow(A).

%!	production_tokens// is nondet.
%
%	Production tokens can be any number of nonterminal or terminal
%	symbols, the terminals following the nonterminals.
production_tokens([Ns|Ts]) --> symbols(nonterminal,Ns), symbols(terminal,Ts).

%!	rule_name(+Name) is nondet.
%
%	A valid rule name is defined by production_name/1
rule_name(Name) --> [Name], { production_name(Name) }.

%!	score// is nondet.
%
%	A score is a list of numbers.
score([[N]|Ns]) --> [[N]], { number(N)}, score(Ns).
score(_) --> [].

%!	dcg_arrow// is nondet.
%
%	The principal operator of dcgs.
dcg_arrow(-->) --> [-->].

%!	symbols// is nondet.
%
%	Symbols can be any number of terminals or nonterminals.
symbols(nonterminal,[S|Ss]) --> [S], {atom(S)}, symbols(nonterminal,Ss).
symbols(nonterminal,[]) --> [].

symbols(terminal,[T|Ts]) --> [T], symbols(terminal,Ts).
symbols(terminal,[]) --> [].



%!	optimisation(Name, Input, Output) is nondet.
%
%	Interface predicate for implementing various optimisations, in
%	other words steps that are not in the algorithm's design but can
%	make things go much faster.
%
%	Obviously, some such things won't be implementable as
%	optimisation/3 clauses. Too bad.
%
optimisation(fully_consumed_example, [[], Temp], Temp):-
	debug(next_example, '~w', ['Dropped one fully consumed example']).
optimisation(fully_consumed_example, [Example, Temp], [Example|Temp]).
