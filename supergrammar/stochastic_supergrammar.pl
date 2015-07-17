:-module(stochastic_supergrammar, [generate_and_test/0
				  %,empty_production_rule/2
				  %,next_token/2
				  ,augmented_production/3
				  %,clear_temporary_productions/0
				  ]).

%:-add_import_module(stochastic_supergrammar, supergrammar, start).

:-use_module(utilities).
:-use_module(configuration).

%!	production_scoring// is det.
%
%	Temporary production term asserted to the database to allow
%	scoring of productions.
:-dynamic
	production_scoring/3.


%!	given_production(?Name,?Production,?Arguments) is  det.
%
%	Dynamic term used to keep track of given productions.
:- dynamic
	given_production/2.

%!	derived_production(?Name,?Production,?Arguments) is  det.
%
%	Dynamic term used to keep track of derived productions.
:- dynamic
	derived_production/3.

%!	termporary_production(?Name) is det.
%
%	Name of the temporary production term used in
%	production_scoring/2.
temporary_production(production_scoring).



%!	given_productions(-Productions) is det.
%
%	A list of all known productions in a grammar of the target
%	language.
%
given_productions(Ps):-
	configuration:rule_complexity(C)
	,configuration:language_module(M)
	% Note this will only get terms with arity //0.
	,findall(P
		,(phrase(M:nonterminal, [N])
		 ,functor(T,N,C)
		 ,clause(M:T,B)
		 ,once(prolog_dcg(T:-B, P)))
	       ,Ps).


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
	given_productions(Ps)
	,forall(member((N --> B), Ps),
		writeln(given_production(N, (N --> B)) ) ).

:-assert_given_productions.



generate_and_test:-
	clear_database
	,assert_unpruned_corpus_length
	% generate_example
	,configuration:example_string(Example)
	% empty_production (implicit)
	% generate_token
	,all_tokens(Example, [T|Tokens])
%	% augment_production
	,once(augmented_production(ypsilon, T, Production))
	,generate_and_test(Tokens,Production,Final_production)
	% score_production
%	,production_score(Augmented, 0, Example, _Score)
	% finalize_production
	,finalize_production(Final_production, New_name)
	,writeln(finalized:Final_production-as:New_name)
%	,finalize_production(Augmented)
	% prune_corpus
	.

generate_and_test([], Production, Production).
generate_and_test([Token|Tokens], Production, Acc):-
	augmented_production(Production, Token, Augmented_production)
	,production_score(Augmented_production, Scored_production)
	,best_scored_production(Production, Scored_production, Best_production)
	,generate_and_test(Tokens, Best_production, Acc).



%best_scored_production(Production, Augmented, Best_production).
best_scored_production((N1, [S1] --> B1), (_N2, [S2] --> _B2), (N1, [S1] --> B1)):-
	S1 > S2
	,!.
best_scored_production(_, Augmented_production, Augmented_production).

% Seems to be working OK.
finalize_production((Name, Score --> Body), (New_name, Score --> Body)):-
	dcg_translate_rule((Name, Score --> Body), (H:-B))
	,rename_rule((H:-B), New_name, Renamed_rule)
	% TESTING: ADD PROPER DERIVATION WHEN TESTED
	,add_new_production(New_name, Renamed_rule, [])
%	,writeln(renamed_rule:Name-as:New_name/Renamed_rule)
	.

rename_rule(_,_,_).
add_new_production(_,_,_).


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



%!	all_tokens(+Example, -Tokens) is det.
%
%	Generate all tokens that are either part of Example or a
%	nonterminal in the grammar of the target language.
%
%	TODO: This is kinda wrong. We may have both terminals and
%	nonterminals as background knowledge encoded in language//0. I
%	need to find a way to add known terminals to the tokens passed
%	to the generate-and-test loop.
%
%	In general the idea is to be able to make best use of a partial
%	background grammar- you can't do this without also using
%	terminals. Although I _do_ want to learn terminals from
%	examples.
%
all_tokens(Example, Tokens):-
	findall(N
	       ,phrase(configuration:language, [N])
	       ,Nonterminals)
	,findall([T] % Add as terminals!
		,member(T, Example)
		,Terminals)
	,diff_list(Nonterminals, Nonterminals_diff, T1)
	,diff_list(Terminals, Terminals_diff, T2)
	,diff_append(Nonterminals_diff-T1, Terminals_diff-T2, Tokens-[]).



%!	grammar(-Start,-Nonterminals,-Terminals,-Productions) is det.
%
%	A grammar, as a quadruple consisting of a Start symbol, a set of
%	Nonterminals, a set of Terminals and a set of Productions.
%
%	@TODO: need a good way to get productions. I'd like to store
%	them in a nice production/2 term with a first argument for the
%	name and a second for the whole production, or even one with
%	args for nonterminals and terminals (and score) but this risks
%	clashing with production/2 here and production/3 in supergrammar
%	module. Also, examples modules will need to declare a clause of
%	that predicate for each given production, otherwise I'll have to
%	do something typically complicated to create those clauses at
%	startup.
%
grammar(Start,Nonterminals,Terminals,Productions):-
	once(phrase(configuration:start, [Start]))
	,findall(N, phrase(configuration:nonterminal, [N]), Nonterminals)
	,findall(T,phrase(configuration:terminal, [T]), Terminals)
	%Empty for now- see todo above.
	,Productions = [].



%!	updated_grammar(+Production,+Grammar,-Updated_grammar) is det.
%
%	Update the current grammar with a newly learned production.
%
updated_grammar(Production, [S,Ns,Ts,Ps], [S,Ns_,Ts_,Ps_]):-
	append(Ps, Production, Ps_)
	,tree_list(Production, Tokens)
	,once(phrase(symbols(nonterminal, P_Ns), Tokens, P_Ts))
	,append(Ns, P_Ns, Ns_)
	,append(Ts, P_Ts, Ts_).



%!	a_production(?Name,?Production,?Constituents) is semidet.
%
%	True when Name is the name of a production term in DCG form:
%	  Name, [Score] --> Nonterminals, Terminals.
%
%	Production is that DCG term and Constituenst is a list of the
%	list of Nonterminals and the list of Terminals in its body.
%
%	On successive backtracking, production/3 will enumerate all
%	productions in the target grammar, both given and derived.
%
a_production(Name,Production,Constituents):-
	given_production(Name,Production,Constituents).

a_production(Name,Production,Constituents):-
	derived_production(Name,Production,Constituents).


given_production(_,_,_).
derived_production(_,_,_).


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
	,once(rule_name(N))
	% Need to check that rule name is unique.
	,initial_score(R).

empty_production((N, [R] --> [])):-
	nonvar(N)
	,production_name(N)
	,initial_score(R).


%!	unpruned_corpus_length(?Length) is det.
%
%	The starting length of the unpruned corpus.
:- dynamic unpruned_corpus_length/1.

%!	assert_unpruned_corpus_length is det.
%
%	Set the length of the unpruned corpus according to the examples
%	in the examples module.
%
%	@TODO: Consider moving to configuration module.
%
assert_unpruned_corpus_length:-
	findall(Example, configuration:example_string(Example), Examples)
	,length(Examples, L)
	,assert(unpruned_corpus_length(L))
	%,compile_predicates([unpruned_corpus_length/1])
	.



%!	production_score(+Production,-Updated_production) is det.
%
%	Update the score of the given production to the proportion of
%	examples it can parse at least partially.
%
production_score((Name, _S --> Body), (Name, [Score] --> Body)):-
	configuration:examples_module(M)
	,dcg_translate_rule((production_scoring(Name) --> Body), R)
	,asserta(M:R)
	,findall(Example
		,(configuration:example_string(Example)
		 ,phrase(M:production_scoring(Name), Example)
		)
		,Parsed)
	,length(Parsed, Parses)
	,unpruned_corpus_length(Unpruned_length)
	,Score is Parses / Unpruned_length.




%!	updated_augmentation_set(+Example,-Augset) is det.
%
%	Build up the set of augmentation terms, as a list of:
%	[Ns,Ts,Ex]
%
%	Where Ns is nonterminals, Ts nonterminals, Ex the tokens from a
%	single example.
%
updated_augmentation_set(Example, Augset):-
	findall(N, phrase(configuration:nonterminal, [N]), Ns)
	,findall(T,phrase(configuration:terminal, [T]), Ts)
	,findall([Token], member(Token, Example),Tokens)
	,append(Ns, Ts, S1)
	,append(S1, Tokens, Augset).



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

augmented_production((Name, Score --> [T|Terminals]), Token, (Name, Score --> Augmented)):-
	augmented_production([], [T|Terminals], Token, [T|Terminals], Augmented).

augmented_production((Name, Score --> Body), Token, (Name, Score --> Augmented)):-
	tree_list(Body, Tokens)
	,once(phrase(symbols(nonterminal, Nonterminals), Tokens, Terminals))
	,augmented_production(Nonterminals, Terminals, Token, Tokens, Augmented).


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
symbols(nonterminal,_) --> [].

symbols(terminal,[T|Ts]) --> [T], symbols(terminal,Ts).
%symbols(terminal,[T]) --> [T],{atom(T)}.
symbols(terminal,_) --> [].



%!	terms_functors(+Terms,?Tail,-Functors) is semidet.
%
%	Convert between a list of compound Terms and a tree of their
%	Functors. Tail is the tail of the tree and can be instantiated
%	to a variable to produce a cyclic term.
%
%	Used to extract the names of terminals from a rule body.
%
%	Example usages:
%
%	==
%	?- Ts = (a(A,B),b(C,D)), terms_functors(Ts,c,TFs).
%	TFs = (a, b, c).
%
%	?- Ts = (a(A,B),b(C,D)), terms_functors(Ts,V,TFs).
%	TFs = (a, b, V).
%
%	?- Ts = (a(A,B),b(C,D)), terms_functors(Ts,[],TFs).
%	TFs = (a, b, []).
%
%	?- Ts = a(A,B), terms_functors(Ts,b,TFs).
%	TFs = (a, b) ;
%	false.
%	==
%
terms_functors(Terms, Tail, Functors):-
	Terms =.. [','|Ts]
	,terms_functors_(Ts, Tail, Functors)
	,!.
terms_functors(Terms, Tail, Functors):-
	Terms =.. [F|_]
	,terms_functors_([F], Tail, Functors).


%!	terms_functors_(+Terms,?Tail,-Functors) is nondet.
%
%	Business end of terms_functors/3.
%
%	Example usages:
%
%	==
%	?- (a(A,B), b(C,D)) =.. [','|Ts], terms_functors(Ts,c,TFs)
%	Ts = [a(A, B), b(C, D)],
%	TFs = (a, b, c)
%	false.
%
%	?- (a(A,B), b(C,D)) =.. [','|Ts], terms_functors(Ts,Tail,TFs).
%	Ts = [a(A, B), b(C, D)],
%	TFs = (a, b, Tail) ;
%	false.
%
%	?- (a(A,B), b(C,D)) =.. [','|Ts], terms_functors(Ts,[],TFs).
%	Ts = [a(A, B), b(C, D)],
%	TFs = (a, b, []) ;
%	false.
%	==
%
terms_functors_([Term], Temp, (Functor,Temp)):-
	Term =.. [Functor|_Args].
terms_functors_([Term|Fs], Temp, Acc):-
	terms_functors_(Fs, Temp, Acc1)
	,terms_functors_([Term], Acc1, Acc).



%!	clear_database is det.
%
%	Clear the database from dynamic terms asserted during a previous
%	run.
%
clear_database:-
	clear_temporary_productions.


%!	clear_temporary_productions is det.
%
%	Remove temporary productions used for scoring from the database.
clear_temporary_productions:-
	configuration:examples_module(M)
	,rule_complexity(C)
	,A is C + 1
	,temporary_production(N)
	,functor(T, N, A)
	,retractall(M:T).


unload_examples_module:-
	configuration:examples_module(Module)
	,forall(current_predicate(Module:P/A)
	       ,abolish(Module:P/A)).

reload_examples_module:-
	configuration:examples_module(Module)
	,consult(Module).

/*
production_score((Name, _S --> Body), (Name, [Score] --> Body)):-
	configuration:examples_module(M)
	,rule_complexity(C)
	,functor(H, Name, C)
	, H =.. [Name|[_|[[_]]]]
	,dcg_translate_rule((Name --> Body), (H:-B))
	,setof(B
	       ,M:B
		,Parses)
	,length(Parses, Score).
*/

/*
finalize_production(Production, Renamed_production):-
	Production = (_Name, Score --> Body)
	,Renamed_production = (New_name, Score --> Body)
	,once(rename_production(Production, Renamed_production))
	,dcg_translate_rule(Renamed_production, Renamed_rule)
	% TESTING: ADD PROPER DERIVATION WHEN TESTED
	,add_new_production(New_name, Renamed_rule, [])
	.

% Copy-pasta from supergrammar module.
rename_production((_, Score --> Body), (New_name, Score --> Body)):-
	rule_name(New_name)
	,configuration:examples_module(M)
	,\+ phrase(M:nonterminal, [New_name])
	.
*/

