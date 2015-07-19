:-module(stochastic_supergrammar, [complete_grammar/1
				  ,augmented_production/3
				  ]).

:-use_module(utilities).
:-use_module(configuration).
:-use_module(library(ordsets)).

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
	derived_production/2.

%!	unpruned_corpus_length(?Length) is det.
%
%	The starting length of the unpruned corpus. Used to calculate
%	production scores.
:- dynamic unpruned_corpus_length/1.

%!	example(?String) is nondet.
%
%	A tokenized example string in the target language.
%	Declared dynamic so that we can remove and re-declare examples
%	as the corpus is pruned.
%
%	@TODO: This is a prime candidate to do in a non-dynamical way.
:-dynamic example/1.

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
%	Called by assert_given_productions/0 so asserted here to allow
%	use as directive.
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
% Dangerous, rather.
given_productions([]).


%!	retract_given_productions is det.
%
%	Clear all given_production/2 terms from the database.
%	Declared here to make it available to
%	assert_given_productions/0.
retract_given_productions:-
	retractall(given_production(_,_)).



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
		assert(given_production(N, (N --> B)) ) ).

:-assert_given_productions.



%!	retract_unpruned_corpus_length is det.
%
%	Clear all unpruned_corpus_length/1 clauses from the database.
%
retract_unpruned_corpus_length:-
	retractall(unpruned_corpus_length(_)).



%!	assert_unpruned_corpus_length is det.
%
%	Set the length of the unpruned corpus according to the examples
%	in the examples module.
%
assert_unpruned_corpus_length:-
	retract_unpruned_corpus_length
	,findall(Example, configuration:example_string(Example), Examples)
	,length(Examples, L)
	,assert(unpruned_corpus_length(L))
	%,compile_predicates([unpruned_corpus_length/1])
	.

:- assert_unpruned_corpus_length.



complete_grammar(Complete_grammar):-
	initialisation(G,Cs)
	,complete_grammar(G, Cs, Cs, Complete_grammar).


%Exit with a new grammar
complete_grammar(G, [], _, G).
	%  For each example in the examples corpus
complete_grammar(G, [[]|Xs], Cs, Acc):-
	complete_grammar(G, Xs, Cs, Acc).
complete_grammar(G, [C|_Xs], Cs, Acc):-
	%  Create a new, originally empty production
%	empty_production(Ypsilon) % could skip with: Ypsilon = ypsilon
	%  [Update] Build the set of augmentation terms
	augmentation_set(C, As)
	%  For each term in the set of augmentation terms
	% [Build up a new production]
	,derived_production(As, Cs, ypsilon, P)
	%  Add the new production to the grammar
	,updated_grammar(P,G,G_)
	%Prune the corpus
	,pruned_corpus(Cs,G_,Cs_)
	%Repeat while there are more examples [in the _un_ pruned corpus]
	,complete_grammar(G_,Cs_,Cs_,Acc).


derived_production([], _Cs, P, P).
	%    Take a new term from the set of augmentation terms
derived_production([A|As], Cs, P, Acc):-
	%    Augment the current production using the new term
	augmented_production(P,A,P_)
	%    Score the production
	%    If the score is 0, discard this version of the production
	%    Otherwise, keep the newest, best scored version of the production
	,best_scored_production(Cs, P, P_, P_best)
	%  Repeat while there are more terms [in the augset]
	,derived_production(As,Cs,P_best,Acc).


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
	grammar(S,Ns,Ts,Ps)
	,examples_corpus(Cs).


%!	examples_corpus(+Examples) is det.
%
%	All examples in the examples corpus.
examples_corpus(Examples):-
	findall(Example,configuration:example_string(Example),Examples).


%!	pruned_corpus(+Corpus,+Production,-Pruned_corpus) is det.
%
%	Remove tokens from each example in the Corpus up to the first
%	token the given Production can't parse and bind the rest to
%	Pruned_corpus.
%
%	Empty examples (ie ones fully parsed) are removed altogether.
pruned_corpus(Corpus, [_S,_Ns,_Ts,Ps], Pruned):-
	pruned_corpus_(Ps,Corpus,Pruned).


%!	pruned_corpus_(+Productions,+Corpus,-Pruned_corpus) is det.
%
%	Business end of pruned_corpus/3. Prune Corpus using eacy
%	Production and bind the result to Pruned_corpus.
%
pruned_corpus_([],Pruned,Pruned).
pruned_corpus_([P|Ps],Corpus,Acc):-
	production_pruned_corpus(Corpus, P, Pruned_corpus)
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
	,production_structure(Production,Name,_Score,Body)
	,dcg_translate_rule(Name --> Body, Rule)
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
	,production_pruned_corpus(Cs, M, R, [Rest|Temp], Acc).
production_pruned_corpus([C|Cs], M, R, Temp, Acc):-
	production_pruned_corpus(Cs, M, R, [C|Temp], Acc).



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
	,grammar_productions(Productions).


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
%	The ste of all Terminals known to the given grammar. Can be [].
%
grammar_terminals(Terminals):-
	% Note the twice-bracketed terminal:
	  setof(T,phrase(configuration:terminal, [[T]]), Terminals)
	; Terminals = [].


%!	grammar_productions(-Productions) is det.
%
%	The set of all Production known to the given grammar. Can be [].
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
updated_grammar((_ --> []), [S,Ns,Ts,Ps], [S,Ns,Ts,Ps]).
updated_grammar((_ --> T), [S,Ns,Ts,Ps], [S,Ns,Ts,Ps]):-
	% New rule for a single nonterminal- discard it.
	atom(T).
updated_grammar(Production, [S,Ns,Ts,Ps], [S,Ns_,Ts_,[(Name, Score --> Tokens)|Ps]]):-
	once(production_structure(Production,Name,Score,Tokens))
	,tree_list(Tokens, Tokens_list)
	,once(phrase(symbols(nonterminal, P_Ns), Tokens_list, P_Ts))
	,[P_Ts_unbracketed] = P_Ts % Don't ask.
	% The Name of the new production is a nonterminal:
	,list_to_ord_set([Name|P_Ns], Ns_ord)
	,list_to_ord_set(P_Ts_unbracketed, Ts_ord)
	,ord_union(Ns_ord, Ns, Ns_)
	,ord_union(Ts_ord, Ts, Ts_).



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
production_structure((Name, Score --> Body), Name,Score,Body).
production_structure((Name --> Body), Name,[],Body).



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
	,once(rule_name(N))
	% Need to check that rule name is unique.
	,initial_score(R).

empty_production((N, [R] --> [])):-
	nonvar(N)
	,production_name(N)
	,initial_score(R).


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
	,best_scored_production(P, P_scored, P_best).


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
best_scored_production(ypsilon, (_, [0] --> _), ypsilon).
best_scored_production(ypsilon, P, P).
best_scored_production((N, [S] --> B), (N, [0] --> _B), (N, [S] --> B)).
best_scored_production((N, [S] --> B), (N, [S_] --> _), (N, [S] --> B)):-
	S > S_.
best_scored_production(Production, _Augmented, Production).


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
scored_production(Corpus, (Name, _ --> Body), (Name, [Score] --> Body)):-
	production_score(Corpus, (Name --> Body), Score)
	,!.
scored_production(Corpus, (Name --> Body), (Name, [Score] --> Body)):-
	production_score(Corpus, (Name --> Body), Score).


%!	production_score(+Production,-Updated_production) is det.
%
%	Update the score of the given production to the proportion of
%	examples it can parse at least partially.
%
production_score(Corpus, (Name --> Body), Score):-
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



%!	augmentation_set(+Example,-Augset) is det.
%
%	Build up the set of augmentation terms, as a list of:
%	[Ns,Ts,Ex]
%
%	Where Ns is nonterminals, Ts nonterminals, Ex the tokens from a
%	single example.
%
augmentation_set(Example, Augset):-
	 grammar_nonterminals(Ns)
	,grammar_terminals(Ts)
	,setof([Token], member(Token, Example),Tokens)
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
symbols(nonterminal,[]) --> [].

symbols(terminal,[T|Ts]) --> [T], symbols(terminal,Ts).
symbols(terminal,[]) --> [].



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


