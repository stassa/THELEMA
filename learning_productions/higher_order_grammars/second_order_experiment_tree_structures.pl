:-module(second_order_experiment_trees, [first_order_phrase/2
				  ,second_order_phrase/2
				  %,start//0
				  %,terminal//0
				  %,nonterminal//0
				  ]).

/*
[debug] 21 ?- phrase(second_order_experiment:destroy_target_t_creature, P), findall(R, (member(P1,P),phrase(second_order_experiment:P1, R)), Rs).
P = [destroy_target, t_creature],
Rs = [[destroy, target], [creature]].
*/

first_order_phrase(Rule, Derivation):-
	phrase(nonterminal, [Rule])
	,atom(Rule)
	,phrase(Rule, Second_order_rules)
	% That'd be nice but we got rules with multiple tokens so?
	,findall(D
		,(member(Second_order_rule, Second_order_rules)
		 ,second_order_phrase(Second_order_rule, D)
		 )
		,Derivation).

second_order_phrase(Rule, Derivation):-
	phrase(nonterminal, [[Rule]])
	,atom(Rule)
	,phrase(Rule, Derivation).

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

destroy_target_t_creature--> [destroy_target, t_creature].









