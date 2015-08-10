:-module(first_order_grammar,[first_order_phrase/3
			     ,second_order_phrase/2
			     ,ability//0
			     ,start//0
			     ,nonterminal//0
			     ,terminal//0
			     ]).

:-multifile start//0
           ,terminal//0
           ,nonterminal//0
	   ,ability//0.

:-use_module(second_order_grammar).

:-edit(first_order_grammar).
:-edit(second_order_grammar).


/*
OK, I guess.

[debug] 50 ?- first_order_phrase(P,D,R).
P = destroy_target_t_artifact,
D = [destroy_target, t_artifact|R] ;
P = destroy_target_t_creature,
D = [destroy_target, t_creature|R] ;
P = t_exile_target_creature,
D = [exile_target_creature|R].
*/
first_order_phrase(Rule, Derivation, Rest_of_sentence):-
	phrase(nonterminal, [Rule])
	,atom(Rule)
	,phrase(Rule, Derivation, Rest_of_sentence).

/*
This is nice:

[debug] 49 ?- notrace, second_order_phrase(P,D).
P = destroy_target_t_artifact,
D = [[destroy, target], [artifact]] ;
P = destroy_target_t_creature,
D = [[destroy, target], [creature]] ;
P = t_exile_target_creature,
D = [[exile, target, creature]].

Though the only way I can see to get non-bracketed terminals is to
flatten the resulting list... or have definitions of first order
nonterminals point to unbracketed tokens, like this:

destroy_target_t_artifact--> [destroy_target, t_artifact].

Which kind of screws the idea that second order nonterminals are first
order terminals :/
	*/

second_order_phrase(Rule, Derivation):-
	phrase(nonterminal, [Rule])
	,atom(Rule)
	,phrase(Rule, Second_order_rules)
	% That'd be nice but we got rules with multiple tokens so?
	,findall(D
		,(member(Second_order_rule, Second_order_rules)
		 ,phrase(Second_order_rule, D)
		 )
		,Derivation).

/*
This won't work because in this scheme, there are no double-bracketed
nonterminals.
second_order_phrase(Rule, Derivation, Rest_of_sentence):-
	phrase(nonterminal, [[Rule]])
	,atom(Rule)
	,phrase(Rule, Derivation, Rest_of_sentence).

% This is OK- but do we really need it?
second_order_phrase(Rule, Derivation):-
	phrase(terminal, [[Rule]])
	,atom(Rule)
	,phrase(Rule, Derivation).
*/

start-->[ability].

% First order terminals- nonterminals in the second order grammar.
terminal-->[[destroy_target]].
terminal-->[[exile_target_creature]].
terminal-->[[t_artifact]].
terminal-->[[t_creature]].

% First order nonterminals- nonterminals in the _first_ order grammar
% (this one)
nonterminal-->[destroy_target_t_artifact].
nonterminal-->[destroy_target_t_creature].
nonterminal-->[t_exile_target_creature].

% start-rule refers to first order nonterminals
ability-->t_exile_target_creature.
ability-->destroy_target_t_artifact.
ability-->destroy_target_t_creature.

% Defintions of first order nonterminals; all in terms of first order
% terminals, a.k.a. second order nonterminals.
t_exile_target_creature--> [exile_target_creature].
destroy_target_t_artifact--> [destroy_target, t_artifact].
destroy_target_t_creature--> [destroy_target, t_creature].











