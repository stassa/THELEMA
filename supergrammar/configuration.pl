:-module(configuration, [rule_complexity/1
			,initial_score/1
			,output_stream/1
			,output_type/1
			,grammar_term/2
			,internal_production_name/1
			,production_scoring_strategy/1
			,dogfooding/1
			]).

%:-register_world(language_simple
%:-register_world(language_mtg_lexicalized
%:-register_world(language_mtg
%:-register_world(examples_mtg_destroy_language_mtg_hand_simulation
%:-register_world(examples_mtg_hand_simulation_language_mtg_hand_simulation
:-register_world(language_mtg_hand_simulation
%:-register_world(language_mtg_hand_simulation_lexicalised
		,[start//0 as start
		 ,terminal//0 as terminal
		 ,nonterminal//0 as nonterminal
		 ]
%		,examples_simple).
%		,examples_mtg_lexicalized).
%		,examples_mtg).
		,examples_mtg_hand_simulation).
%		,examples_mtg_destroy).
%		,examples_mtg_destroy_short).
%		,examples_mtg_all_destroy_one_sentence_per_line).


%!	nonterminal_arity(?Arity) is semidet.
%
%	Rule head arity, including variables automatically added by
%	dcg_translate_rule/2 during grammar clause expansion.
rule_complexity(2).

%!	initial_score(?P) is det.
%
%      Starting score of a new production.
initial_score(-1).

%!	output_stream(?Stream) is det.
%
%	The name of the (probably file) stream to which we should write
%	output of an induction attempt.
%
%	@TODO: Build the output file name up from the configured
%	language or examples module. Cause more fun.
%
output_stream(output(Output_file_name)):-
	examples_module(M_ex)
	,language_module(M_lang)
	,production_scoring_strategy(S)
	,atomic_list_concat([M_lang,M_ex,S],'_',Name)
	,output_type(T)
	,output_format(T,Ext)
	,atom_concat(Name, Ext, Output_file_name).

%!	output_type(?Type) is det.
%
%	The type of file to write as output of the main loop
%	(complete_grammar/0 at this time).
%
%	Type is one of:%
%	* loose. Write grammar elements in a loose format, with the
%	Start symbol, lists of Terminals and Nonterminals and each
%	production on a separate line.
%	* terms. Write out a valid Prolog file. Each element of the
%	grammar that is wrapped in an appropriate term where necessary:
%	start_symbol(S), terminals_set(Ts) and nonterminals_set(Ns).
%	Productions are written	as DCG rules, as derived.
%	* grammar. As terms but will write out a language module file
%	conforming to the specification in register_world/2 (in other
%	words, the module should export the predicates in the reexport
%	list of register_world/2).
%
%	The 'grammar' option connects each terminal in the output
%	grammar to an appropriate DCG rule.
%
%	The grammar's start symbol is connected to start//0, each
%	terminal is connected to terminal//0 and each nonterminal to
%	nonterminal//0. Finally, each derived production is connected to
%	the start symbol.
%
%	Terminals are wrapped in double-brackets and nonterminals are
%	wrapped in single brakcets to indicate that terminal//0 and
%	nonterminal//0 are not meant to derive or generate sentences in
%	the langauge and instead act as references to the respective
%	categories of tokens in the grammar.
%
%	The end result is (should be) a complete grammar that can be
%	used to parse a phrase in the target language, starting from
%	start//0, as follows:
%	==
%	?- phrase(start, [S]), phrase(S, D).
%	==
%
%	Where on successive backtracking each derivation of the grammar
%	will be bound to D.
%
output_type(grammar).

%!	output_format(?Type,?Extension) is nondet.
%
%	The Extension that goes with the currently configured output
%	Type.
%
output_format(loose, '.log').
output_format(terms, '.pl').
output_format(grammar, '.pl').


%!	grammar_term(?Member,?Name) is nondet.
%
%	Functor names for members of the derived Grammar.
%	Member is one of:
%	* s for the grammar's start symbol
%	* t for the set of terminals
%	* n for the set of nonterminals
%	* p for the set of productions
%
grammar_term(s, start_symbol).
grammar_term(t, terminals).
grammar_term(n, nonterminals).
grammar_term(p, productions).


%!	internal_production_name(?E) is nondet.
%
%	A dumny production name to give productions we are in the
%	process of deriving.
%
internal_production_name(ypsilon).


%!	production_scoring_strategy(?Strategy) is det.
%
%	How to score newly-augmented productions, to select the best.
%	One of:
%	* parsed
%	* tokens
%	* tokens_over_parsed
%
%	With "parsed" selected, a newly augmented production is scored
%	with P/C where P the number of examples the production can parse
%	at least partially and C the number of examples in the original,
%	unpruned corpus.
%
%	With "tokens", the score is the best value of P/T where P the
%	number of tokens of a example the production has parsed and T
%	the number of tokens in that example. To clarify, the end result
%	is that the production is scored with the highest P/T ratio it
%	achieved over the (unpruned) corpus.
%
%	With "tokens_over_parsed" the score is S/C where C the number of
%	examples in the unpruned corpus and S the sum of the value P/T
%	for each example, ie the ratio of tokens parsed over the number
%	of tokens of each example. Or in other words:
%	==
%	N
%	S   p/t(i)
%	i=1
%	==
%
%	Where t(i) the number of tokens in the currently parsing example
%	and p the proportion of tokens of this exapmle parsed by the
%	production.
%
production_scoring_strategy(tokens_over_parsed).

%!	dogfooding(+Boolean) is det.
%
%	Whether to keep derived grammar elements for subsequent runs of
%	the main loop or not.
%
dogfooding(false).
