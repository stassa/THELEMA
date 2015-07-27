:-module(configuration, [rule_complexity/1
			,initial_score/1
			,output_stream/1
			,output_type/1
			,grammar_term/2
			,internal_production_name/1
			]).

%:-register_world(language_simple
%:-register_world(language_mtg_lexicalized
%:-register_world(language_mtg
:-register_world(language_mtg_hand_simulation
%:-register_world(language_mtg_hand_simulation_lexicalised
		,[%language//0 as language
		  start//0 as start
		 ,terminal//0 as terminal
		 ,terminals//0 as terminals
		 ,nonterminal//0 as nonterminal
		 ,nonterminals//0 as nonterminals
		 ]
%		,examples_simple).
%		,examples_mtg_lexicalized).
%		,examples_mtg).
		,examples_mtg_hand_simulation).
%		,examples_mtg_destroy).


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
	,atomic_list_concat([M_ex,M_lang],'_',Name)
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
%	* dogfooding. As terms but will write out a language module file
%	conforming to the specification in register_world/2 (in other
%	words, the module should export the predicates in the reexport
%	list of register_world/2).
%
%	The dogfooding option connects each terminal in the output
%	grammar to an appropriate DCG rule.
%
%	The grammar's start symbol is connected to start//0, each
%	terminal is connected to terminal//0 and each nonterminal to
%	nonterminal//0. Finally, terminals//0 and nonterminals//0 are
%	connected to a rule mapping it to the terminals and nonterminals
%	in the grammar.
%
%	The end results is (should be) a complete grammar that can be
%	used to parse a phrase in the target language, starting from
%	start//0.
%
output_type(dogfooding).

%!	output_format(?Type,?Extension) is nondet.
%
%	The Extension that goes with the currently configured output
%	Type.
%
output_format(loose, '.log').
output_format(terms, '.pl').
output_format(dogfooding, '.pl').


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
