:-module(stochastic_supergrammar, []).

:-add_import_module(stochastic_supergrammar, supergrammar, start).


generate(_Rule_complexity,_Derivation_length,_Inference_limit,_Options):-
	clear_productions
	% take the next example
	,configuration:example_string(Example)
	% create a new rule
	% set its probability to 0
	,empty_production(Production:Probability)
	% take the next token (terminal or nonterminal)
	% augment the new rule
	,augmented_production(Production:Probability, Example, _Augmented)
	% calculate its probability
	% compare the two probabilities
	% Choose a rule (new or old)
	% repeat from augment step
	% prune the examples
	% repeat from the top
	% exit if there are no more examples.
	.


empty_production(P:0):-
	once(rule_name(N))
	,dcg_translate_rule(N --> [], P).

%!	augmented_production(+Rule:Probability,+Example,Augmented:Probability) is nondet.
%
%	Augment the rule with a single terminal or nonterminal (the
%	termial is a token from the given Example)
augmented_production((H:-T):Probability, _, P:Probability):-
	phrase(language, [D])
	,T =.. [Name|Args]
	,reverse([D|Args], New) % Place new token at right
	,Tt =.. [Name|New]
	,dcg_translate_rule(H --> Tt, P).
augmented_production((H:-T):Probability, Example, P:Probability):-
	member(E, Example)
	,T =.. [Name|Args]
	,reverse([E|Args], New) % Place new token at right
	,Tt =.. [Name|New]
	,dcg_translate_rule(H --> Tt, P).

