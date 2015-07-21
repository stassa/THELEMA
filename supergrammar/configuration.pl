:-module(configuration, [rule_complexity/1
			,initial_score/1
			,output_stream/1]).


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
%output_stream('grammar.log').

output_stream(output(Output_file_name)):-
	examples_module(M_ex)
	,language_module(M_lang)
	,atomic_list_concat([M_ex,M_lang],'_',Name)
	,atom_concat(Name, '.log', Output_file_name).


:-register_world(language_simple
%:-register_world(language_mtg_lexicalized
%:-register_world(language_mtg
%:-register_world(language_mtg_hand_simulation
		,[language//0 as language
		 ,start//0 as start
		 ,terminal//0 as terminal
		 ,terminals//0 as terminals
		 ,nonterminal//0 as nonterminal
		 ,nonterminals//0 as nonterminals
		 ]
		,examples_simple).
%		,examples_mtg_lexicalized).
%		,examples_mtg).
%		,examples_mtg_hand_simulation).
%		,examples_mtg_destroy).

