:-use_module(stochastic_supergrammar).

:-begin_tests(production_structure).

/*
% Tests are explicitly deterministic, to point out that you can't end up
% in this sort of situation on backtracking:
?- production_structure((n,[0.1]-->[t_1]),N,S,Ts), production_structure(P,N,[],Ts).
N = n, S = [0.1], Ts = [t_1], P = (n-->[t_1]) ;
N = n, S = [0.1], Ts = [t_1], P = (n, []-->[t_1]). % specifically this!
*/

test(production_structure_in_out_no_score, []):-
	production_structure((nm --> n, [t_1]), N, S, B)
	,N = nm
	,S = []
	,B = (n, [t_1]).

test(production_structure_in_out_name_score_body, []):-
	production_structure((nm, [0.5] --> n, [t_1]), N, S, B)
	,N = nm
	,S = [0.5]
	,B = (n, [t_1]).

test(production_structure_out_in_no_score, []):-
	production_structure(P, nm, [], (n,[t_1]))
	,P = (nm --> n, [t_1]).

test(production_structure_out_in_name_score_body, []):-
	production_structure(P, nm, [0.6], (n,[t_1]))
	,P = (nm, [0.6] --> n, [t_1]).

:-end_tests(production_structure).


:-begin_tests(best_scored_production).

/*
Seems like all the augmented productions are based on a production
scored with [-1] which they carry out as the augmented form's score.
This turns out to be convenient to do away with an error: the score of
the original form is _not_ the score of the augmented form and should
really be _discarded_ until the augmented form is scored. In versions
of the program so far, the score is indeed discarded but only inside
scored_production/3. There's no reason to keep it around until then and
a new production should really be getting the default score (-1 so far).

Needless to say, I'll need to add tests that at least demonstrate this
is the case. So: TODO. To do this that is.
*/

% Zero scores and ypsilon.

% Only semidet really.
test(best_scored_production_ypsilon_ypsilon, [nondet]):-
	stochastic_supergrammar:best_scored_production(ypsilon, ypsilon, ypsilon).

% Semidet.
test(best_scored_production_ypsilon_zero_scored, [nondet]):-
	stochastic_supergrammar:best_scored_production(ypsilon, (p, [0] --> n,[t]), ypsilon).

% Semidet.
test(best_scored_production_other_zero_scored, [nondet]):-
	Production = (p, [-1] --> n,[t_1])
	,stochastic_supergrammar:best_scored_production(Production, (p, [0] --> n,[t]), Best)
	,Best = Production.

% Actually nondeterministic but both results are same.
test(best_scored_production_zero_scored_first_arg, [nondet]):-
	Production = (p, [-1] --> n,[t_1])
	,stochastic_supergrammar:best_scored_production((p, [0] --> n,[t]), Production, Best)
	,Best = Production.


% Straight up comparisons

test(best_scored_production_augmented_best, [nondet]):-
	Production = (p, [-1] --> n,[t])
	,Augmented = (p, [0.1] --> n,[t_1])
	,stochastic_supergrammar:best_scored_production(Production, Augmented, Best)
	,Best = Augmented.

test(best_scored_production_pre_augmented_best, [nondet]):-
	Production = (p, [0.1] --> n,[t])
	,Augmented = (p, [-1] --> n,[t_1])
	,stochastic_supergrammar:best_scored_production(Production, Augmented, Best)
	,Best = Production.

test(best_scored_production_equal_chose_augmented, [nondet]):-
	Production = (p, [0.1] --> n,[t])
	,Augmented = (p, [0.1] --> n,[t_1])
	,stochastic_supergrammar:best_scored_production(Production, Augmented, Best)
	,Best = Augmented.


% Fail tests.

test(best_scored_production_best_first_arg, [fail]):-
	Production = (p, [-1] --> n,[t])
	,Augmented = (p, [0.1] --> n,[t_1])
	,stochastic_supergrammar:best_scored_production(Production, Augmented, Best)
	,Best = Production.

test(best_scored_production_equal_chose_production, [fail]):-
	Production = (p, [0.1] --> n,[t])
	,Augmented = (p, [0.1] --> n,[t_1])
	,stochastic_supergrammar:best_scored_production(Production, Augmented, Best)
	,Best = Production.

test(best_scored_production_zero_scored_as_best, [fail]):-
	Production = (p, [-1] --> n,[t_1])
	,Zero_scored = (p, [0] --> n,[t])
	,stochastic_supergrammar:best_scored_production(Production, Zero_scored, Zero_scored).

test(best_scored_production_zero_scored_first_arg_as_best, [fail]):-
	Production = (p, [-1] --> n,[t_1])
	,Zero_scored = (p, [0] --> n,[t])
	,stochastic_supergrammar:best_scored_production(Zero_scored, Production, Zero_scored).

% This should not happen, but just in case.
test(best_scored_production_other_zero_scored, [fail]):-
	Zero_scored = ([0] --> n,[t])
	,stochastic_supergrammar:best_scored_production(Zero_scored, Zero_scored, Zero_scored).

:-end_tests(best_scored_production).


:-begin_tests(augmented_production).

% ========  Legal augmentations  ========

% Tests for case: [] -> n | t (empty production augmented by any token)
test(augmented_production_augment_empty_production_with_a_nonterminal, [nondet]):-
	augmented_production(ypsilon, g1, (Name, [-1] --> g1))
	,stochastic_supergrammar:production_name(Name).

test(augmented_production_augment_empty_production_with_a_terminal, [nondet]):-
	augmented_production(ypsilon, [a], Production)
	,Production = (Name, [-1] --> [a])
	,stochastic_supergrammar:production_name(Name).

% Tests for case: n+ -> n | t.
test(augmented_production_augment_single_nonterminal_with_a_nonterminal,[nondet]):-
	augmented_production((a0, [-1] --> g1), g2, Augmented)
	,Augmented = (a0, [-1] --> g1, g2).

test(augmented_production_augment_single_nonterminal_with_a_terminal,[nondet]):-
	augmented_production((a0, [-1] --> g1), [a], Augmented)
	,Augmented = (a0, [-1] --> g1, [a]).

test(augmented_production_augment_nonterminals_with_a_nonterminal, [nondet]):-
	augmented_production((a0, [-1] --> g1, g2), g3, Augmented)
	,Augmented = (a0, [-1] --> g1, g2, g3).

test(augmented_production_augment_nonterminals_with_a_terminal, [nondet]):-
	augmented_production((a0, [-1] --> g1, g2), [a], Augmented)
	,Augmented = (a0, [-1] --> g1, g2, [a]).

% Tests for case: n* t+ -> n | t
test(augmented_production_augment_nonterminals_and_single_terminal_with_a_terminal):-
	augmented_production((a0, [-1] --> g1, g2, [a]), [b], Production)
	,Production = (a0, [-1] --> g1, g2, [a,b]).

test(augmented_production_augment_nonterminals_and_terminals_with_a_terminal):-
	augmented_production((a0, [-1] --> g1, g2, [a, b]), [c], Augmented)
	,Augmented = (a0, [-1] --> g1, g2, [a, b, c]).

test(augmented_production_augment_single_terminal_with_a_terminal,[]):-
	augmented_production((a0, [-1] --> [a]), [b], Augmented)
	,!
	,Augmented = (a0, [-1] --> [a,b]).

test(augmented_production_augment_terminals_with_a_terminal,[]):-
	augmented_production((a0, [-1] --> [a,b]), [c], Augmented)
	,!
	,Augmented = (a0, [-1] --> [a,b,c]).


% ======== Illegal augmentations. ========

% Can't add nonterminals to right of terminal.
test(augmented_production_augment_nonterminals_and_single_terminal_with_a_nonterminal,[fail]):-
	augmented_production((a0, [-1] --> g1, g2, g3, [a]), g4, _).

% Again: nonterminals may not follow a terminal
test(augmented_production_augment_nonterminals_and_terminals_with_a_nonterminal,[fail]):-
	augmented_production((a0, [-1] --> g1, g2, [a, b]), g3, Augmented)
	,Augmented = (a0, [-1] --> g1, g2, g3, [a, b]).

% Nonterminals must precede terminals!
test(augmented_production_augment_single_terminal_with_a_nonterminal,[fail]):-
	augmented_production((a0, [-1] --> [a]), g1, _).


% ======== Tests for ancestry verification. ======== %

% Verify augmentation of empty production.
test(augmented_production_verify_augmenting_empty_production_with_a_nonterminal, []):-
	augmented_production(ypsilon, [a], (a0, [-1] --> [a])).

test(augmented_production_verify_augmenting_empty_production_with_a_terminal, []):-
	augmented_production(ypsilon, g1, (a0, [-1] --> g1)).

% Tests for verifying augmentation in case: n+ -> n | t.
test(augmented_production_verify_augmenting_single_nonterminal_with_a_nonterminal, [nondet]):-
	augmented_production((a0, [-1] --> g1), g2, (a0, [-1] --> g1, g2)).

test(augmented_production_verify_augmenting_single_nonterminal_with_a_terminal,[nondet]):-
	augmented_production((a0, [-1] --> g1), [a], (a0, [-1] --> g1, [a])).

test(augmented_production_verify_augmenting_nonterminals_with_a_nonterminal, [nondet]):-
	augmented_production((a0, [-1] --> g1, g2), g3, (a0, [-1] --> g1, g2, g3)).

test(augmented_production_verify_augmenting_nonterminals_with_a_terminal, [nondet]):-
	augmented_production((a0, [-1] --> g1, g2), [a], (a0, [-1] --> g1, g2, [a])).

% Tests for verifying augmentation in case: n* t+ -> n | t
test(augmented_production_verify_augmenting_nonterminals_with_a_terminal, []):-
	augmented_production((a0, [-1] --> g1, g2, [a]), [b], (a0, [-1] --> g1, g2, [a,b])).

test(augmented_production_verify_augmenting_nonterminals_and_terminals_with_a_terminal, []):-
	augmented_production((a0, [-1] --> g1, g2, [a, b]), [c], (a0, [-1] --> g1, g2, [a, b, c])).

test(augmented_production_verify_augmenting_single_terminal_with_a_terminal,[nondet]):-
	augmented_production((a0, [-1] --> [a]), [b], (a0, [-1] --> [a,b])).

test(augmented_production_verify_augmenting_terminals_with_a_terminal,[nondet]):-
	augmented_production((a0, [-1] --> [a,b]), [c], (a0, [-1] --> [a,b,c])).

% Tests for inferring ancestral token when augmenting empty production.
test(augmented_production_infer_ancestral_nonterminal_from_empty_production_and_augmented_production, []):-
	augmented_production(ypsilon, Token, (a0, [-1] --> g1))
	,!
	,Token = g1.

test(augmented_production_infer_ancestral_terminal_from_empty_production_and_augmented_production, []):-
	augmented_production(ypsilon, Token, (a0, [-1] --> [a]))
	,!
	,Token = [a].

% Tests for inferring ancestral token in case:  n+ -> n | t.
test(augmented_production_infer_ancestral_token_from_single_nonterminal_augmented_with_a_nonterminal, []):-
	augmented_production((a0, [-1] --> g1), Token, (a0, [-1] --> g1, g2))
	,!
	,Token = g2.

test(augmented_production_infer_ancestral_token_from_single_nonterminal_augmented_with_a_terminal,[]):-
	augmented_production((a0, [-1] --> g1), Token, (a0, [-1] --> g1, [a]))
	,!
	,Token=[a].

test(augmented_production_infer_ancestral_token_from_nonterminals_augmented_with_a_nonterminal, []):-
	augmented_production((a0, [-1] --> g1, g2), Token, (a0, [-1] --> g1, g2, g3))
	,!
	,Token=g3.

test(augmented_production_infer_ancestral_token_from_nonterminals_augmented_with_a_terminal, []):-
	augmented_production((a0, [-1] --> g1, g2), Token, (a0, [-1] --> g1, g2, [a]))
	,!
	,Token=[a].

% Tests for inferring ancestral token in case: n* t+ -> n | t
test(augmented_production_infer_ancestral_token_from_nonterminals_augmented_with_a_terminal, []):-
	augmented_production((a0, [-1] --> g1, g2, [a]), Token, (a0, [-1] --> g1, g2, [a,b]))
	,!
	,Token=[b].

test(augmented_production_infer_ancestral_token_from_nonterminals_and_terminals_augmented_with_a_terminal, []):-
	augmented_production((a0, [-1] --> g1, g2, [a, b]), Token, (a0, [-1] --> g1, g2, [a, b, c]))
	,!
	,Token=[c].

test(augmented_production_infer_ancestral_token_from_single_terminal_augmented_with_a_terminal,[]):-
	augmented_production((a0, [-1] --> [a]), Token, (a0, [-1] --> [a,b]))
	,!
	,Token=[b].

test(augmented_production_infer_ancestral_token_from_terminals_augmented_with_a_terminal,[]):-
	augmented_production((a0, [-1] --> [a,b]), Token, (a0, [-1] --> [a,b,c]))
	,!
	,Token=[c].

% ========== Illegal mode tests: ========== %

test(augmented_production_infer_ancestral_token_from_single_nonterminal_augmented_with_a_nonterminal
    , [throws(error(instantiation_error, _))]):-
	augmented_production(Body, g2, (a0, [-1] --> g1, g2))
	,!
	,Body = (a0, [-1] --> g1).


% Not too much point testing this- augmented production doesn't handle
% Scores; they're bound directly to the head of
% augmented_production/3 clauses and carried out again unmodified.
test(augmented_production_infer_ancestral_score_from_empty_production_and_augmented_production
    ,[blocked('Not much point in testing this')]):-
	augmented_production(ypsilon, g1, (a0, Score --> g1))
	,!
	,Score = [-1].

:-end_tests(augmented_production).














