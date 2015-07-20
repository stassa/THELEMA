:-use_module(stochastic_supergrammar).

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














