:-module(language_mtg_hand_simulation_partial, [start//0
					       ,terminal//0
					       ,nonterminal//0
					       ]).
:-add_import_module(language_mtg_hand_simulation_partial, language, start).

:-dynamic nonterminal//0.

start_symbol --> [ability].

%language_mtg_hand_simulation:nonterminal --> [stassa].
%stassa --> [stassa].

language_mtg_hand_simulation_partial:nonterminal --> [mtg_verb].
language_mtg_hand_simulation_partial:nonterminal --> [target_expression].
language_mtg_hand_simulation_partial:nonterminal --> [permanent].

ability --> mtg_verb, target_expression.

mtg_verb --> [destroy].
mtg_verb --> [exile].

target_expression --> [target], permanent.

permanent --> [creature].
permanent --> [artifact].

/*
given_production(mtg_verb,mtg_verb --> [destroy]).
given_production(mtg_verb,mtg_verb --> [exile]).
given_production(target_expression,(target_expression --> [target], permanent)).
given_production(permanent,permanent --> [creature]).
given_production(permanent,permanent --> [artifact]).
*/

/*
1 ?- M=language_mtg_hand_simulation_partial, phrase(M:start, [S]), phrase(M:S, P).
M = language_mtg_hand_simulation_partial,
S = ability,
P = [destroy, target, creature] ;
M = language_mtg_hand_simulation_partial,
S = ability,
P = [destroy, target, artifact] ;
M = language_mtg_hand_simulation_partial,
S = ability,
P = [exile, target, creature] ;
M = language_mtg_hand_simulation_partial,
S = ability,
P = [exile, target, artifact].
*/
