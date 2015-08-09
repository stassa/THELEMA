:-module(second_order_experiment, [first_order_phrase/3
				  ,second_order_phrase/3
				  ,start//0
				  ,terminal//0
				  ,nonterminal//0]).

first_order_phrase(Rule, Derivation, Rest_of_sentence):-
	phrase(nonterminal, [Rule])
	,atom(Rule)
	,phrase(Rule, Derivation, Rest_of_sentence).

second_order_phrase(Rule, Derivation, Rest_of_sentence):-
	phrase(nonterminal, [[Rule]])
	,atom(Rule)
	,phrase(Rule, Derivation, Rest_of_sentence).

start --> [ability].

% Second order terminals:
terminal-->[[creature]].
terminal-->[[destroy]].
terminal-->[[target]].

% First order terminals:
terminal-->[destroy_target].
terminal-->[t_creature].

% Second order nonterminals
nonterminal-->[[destroy_target]].
nonterminal-->[[t_creature]].

% First order nonterminals
nonterminal-->[destroy_target_t_creature].

% Second order productions
t_creature-->[creature].
destroy_target-->[destroy, target].

% First order productions
ability-->destroy_target_t_creature.

destroy_target_t_creature--> destroy_target, t_creature.


