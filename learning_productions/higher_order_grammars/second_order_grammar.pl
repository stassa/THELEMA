:-module(second_order_grammar,[exile_target_creature//0
			      ,t_artifact//0
			      ,t_creature//0
			      ,destroy_target//0
			      ]).
/*
:-multifile first_order_grammar:start//0
           ,first_order_grammar:terminal//0
           ,first_order_grammar:nonterminal//0
	   ,first_order_grammar:ability//0.

first_order_grammar:start-->[ability].

% Second order terminals- terminals in (this) second order grammar.
% _NOT_ define and NOT referred to in the first order grammar; it
% doesn't know them. Never did. Never will.
first_order_grammar:terminal-->[[artifact]].
first_order_grammar:terminal-->[[creature]].
first_order_grammar:terminal-->[[destroy]].
first_order_grammar:terminal-->[[exile]].
first_order_grammar:terminal-->[[target]].

% Second order nonterminals- nonterminals in (this) second order
% grammar AND terminals in the first order grammar.
first_order_grammar:nonterminal-->[destroy_target].
first_order_grammar:nonterminal-->[exile_target_creature].
first_order_grammar:nonterminal-->[t_artifact].
first_order_grammar:nonterminal-->[t_creature].

% start-rule refers to secodn order nonterminals; maybe it shouldn't?
% Will we need to get these? They're not really _full_ abilities are
% they?
first_order_grammar:ability-->exile_target_creature.
first_order_grammar:ability-->t_artifact.
first_order_grammar:ability-->t_creature.
first_order_grammar:ability-->destroy_target.
*/
% Definitions of second order nonterminals a.k.a. first order terminals;
% all in terms of second order terminals, a.k.a. nothing the first order
% grammar knows or can see. Well- it can access them but.
exile_target_creature-->[exile, target, creature].
t_artifact-->[artifact].
t_creature-->[creature].
destroy_target-->[destroy, target].
