:-module(configuration, [rule_complexity/1
			,initial_score/1
			,assert_given_productions/0
			,prolog_dcg/2
			]).

/** <module> Configuration for supergrammar generate-and-test cycles.

Note: this needs updating after changing this module to configure a
language module and an example module separate from each other
(although pertaining to the same world).

This module allows the user to plug in a different examples module
with its own language and example strings without having to modify
the export list of each example module to avoid namespace clashes.
These occur if you try for example to load two modules that export a
predicate named language//0 at the same time. register_examples/2
declared herein sidesteps this by reexporting predicates (keeping
their names).

All that said some naming conventions must be observed for this
module to work as it is meant to.

An example module must export the following predicates:

* language//0.
  A specification of the type of the target langauge.
* terminal//0.
  A terminal in the target language.
* terminals//0.
  A set of zero or more terminals of the target language.
* nonterminal//0.
  A nonterminal symbol in the target language.
* nonterminals//0.
  A set of zero or more nonterminal symbols of the target language.
* example_string/1.
  A set of positive examples of strings in the target language.

By "specification of the target language" we mean that derivations of
language//0 are not strings in the target language, rather they are
productions in the grammar of the target language.

Together terminals//0, terminal//0, nonterminals//0 and nonterminal//0
make up a partial grammar that represents background knowledge of the
target language. Each of those may be empty. The plural versions
(nonterminals//0 and terminals//0) are there for convenience and should
just give the full set of non/terminals in the language.

Additionally, each language may export one or more grammar rules with
the same names as the terminal and nonterminal symbols defined in the
rules outlined above.

For instance, if a language file declares a terminal//0 clause:

nonterminal --> [g1].

Then it should also declare a rule for that nonterminal, for example:

g1 --> [a,b,c].


@TODO: Complete this.

@TODO: make this configurable outside Prolog - eg, read in the name of
the examples module from an xml or other config file.

 */

:-use_module(utilities, [diff_list/3
			,tree_list/2
			,list_tree/2]).

%!	examples_module(?Examples_module).
%
%	Dynamic term
:- dynamic examples_module/1.

:- dynamic language_module/1.

%!	given_production(?Name,?Production,?Arguments) is  det.
%
%	Dynamic term used to keep track of given productions.
:- dynamic
	given_production/3.

%!	derived_production(?Name,?Production,?Arguments) is  det.
%
%	Dynamic term used to keep track of derived productions.
:- dynamic
	derived_production/3.

%!	nonterminal_arity(?Arity) is semidet.
%
%	Rule head arity, including variables automatically added by
%	dcg_translate_rule/2 during grammar clause expansion.
rule_complexity(2).

%!	initial_score(?P) is det.
%
%      Starting score of a new production.
initial_score(-1).



%!	register_world(+Language_module,+Language_predicates,+Examples_module) is det.
%
%	Reexport Language_module renaming each predicate in the list
%	of Language_predicates to avoid name clashes. Also reexport
%	Examples_module renaming example_string/1 to example_string/1.
%	Hack to convince Swi to load a different module simply by
%	modifying and consulting this file.
%
%	Note: this works in Swi 7.* but I can't guarantee it will work
%	in anything earlier than that.
%
register_world(Language_module, Renamed_predicates, Examples_module):-
	register_language(Language_module, Renamed_predicates)
	,register_examples(Examples_module).



%!	register_language(+Language_module,+Language_predicates) is det.
%
%	Reexport Language_module renaming each predicate in the list
%	of Language_predicates to avoid name clashes.
%
%	Language_predicates should be a list of terms:
%	Predicate_indicator as New_predicate_name
%
%	Where Predicate_indicator is a name/arity term for a DCG rule:
%	Name//Arity
%
%	The new name can be the same as the current name- this is a bit
%	of a hack but the effect is that we can load a different module
%	just by modifying and then consulting this file (rather than
%	having to exit).
%
%	@SEE register_world.
%
register_language(Language_module, Renamed_predicates):-
	reexport(Language_module, except(Renamed_predicates))
	,(  language_module(_)
	->  retract(language_module(_))
	;   true
	)
	,assert(language_module(Language_module))
	,! % Green cut
	.



%!	register_examples(+Examples_module) is det.
%
%	Reexport Examples_module renaming example_string/1 to
%	example_string/1. Same hack as for register_language/2.
%
register_examples(Examples_module):-
	reexport(Examples_module, except([example_string/1 as example_string]))
	,(  examples_module(_)
	->  retract(examples_module(_))
	;   true
	)
	,assert(examples_module(Examples_module))
	,! % Green cut
	.


:-register_world(language_simple
%:-register_world(language_mtg_lexicalized
%:-register_world(language_mtg
%:-register_world(language_mtg_hand_simulation
		,[language//0 as language
		 ,terminal//0 as terminal
		 ,terminals//0 as terminals
		 ,nonterminal//0 as nonterminal
		 ,nonterminals//0 as nonterminals
		 ]
		,examples_simple).
%		,examples_mtg_lexicalized).
%		,examples_mtg).
%		,examples_mtg_hand_simulation).


assert_given_productions:-
	rule_complexity(C)
	,language_module(M)
	,forall((phrase(M:nonterminal, [N])
		 ,functor(T,N,C)
		,clause(M:T,B)
		)
		%,assert(given_production(N,B))
		,writeln(given_production(N,B))
	       ).


%	prolog_dcg(+Prolog,-DCG) is det.
%
%	Convert between a grammar rule in normal Prolog form and its
%	equivalent DCG notation.
%
prolog_dcg(Head:-true, (Name --> Ts_Diff)):-
	! % Green cut- avoids errors in next clause when the rule has only
	% terminals on the right hand side.
	,Head =.. [Name|[Ts|_]]
	,diff_list(Ts, Ts_Diff, []).
prolog_dcg(Head:-Body, (Name --> Tokens_)):-
	Head =.. [Name|[Head_args|_]]
	,tree_list(Body, Body_args)
	,(   \+ var(Head_args)
	 ->  diff_list(Head_args,Head_args_,[])
	   ,append([Head_args_],Body_args,Args)
	;   Args = Body_args
	)
	,phrase(production_term(Tokens), Args)
	,list_tree(Tokens, Tokens_).


%!	production_term(-Tokens)// is nondet.
%
%	A short DCG production grammar (as in, a grammar for Definite
%	Clause Grammar productions). The single argument gathers a list
%	of all tokens in the right-hand side of the rule.
production_term(Ts) --> tokens(Ts).

%!	tokens(-Tokens)// is nondet.
%
%	The list of Tokens in the input rule.
tokens([]) --> [].
tokens([T|Ts]) --> nonterminal_term(T),tokens(Ts).
tokens([T|Ts]) --> terminals_list(T),tokens(Ts).

%!	nonterminal_term(-Nonterminals)// is nondet.
%
%	A Nonterminal token in the right-hand side of the input rule.
nonterminal_term(N) --> [T], { T \= [_|_], functor(T,N,_), N \== =}.

%!	terminals_list(-Terminals)// is nondet.
%
%	A list of (one or more) terminals in the right-hand side of the
%	input rule.
%
%	There's a bit that probably looks arcane in there; this one:
%	==
%	[_=Ts]
%	==
%
%	That's there because when a list of terminals follows any number
%	of tokens in the right-hand side of a rule it's unified with the
%	difference-variable of the last token. The unification is done
%	using =\2 and we want to get rid of it to only keep the meat and
%	potatoes, ie the actual list of terminals.
%
%	Hence, that arcane bit.
%
terminals_list(Ls) --> [_=Ts], {diff_list(Ts,Ls,[])}.
terminals_list(Ts) --> [Ts], {is_list(Ts)}.


:-begin_tests(prolog_dcg).

test(prolog_dcg_single_terminal, []):-
	DCG = (single_terminal --> [t])
	,dcg_translate_rule(DCG, R)
	,once(prolog_dcg(R, DCG)).

test(prolog_dcg_terminals, []):-
	DCG = (terminals --> [t_1,t_2,t_3])
	,dcg_translate_rule(DCG, R)
	,once(prolog_dcg(R, DCG)).

test(prolog_dcg_single_nonterminal, []):-
	DCG = (single_nonterminal --> n)
	,dcg_translate_rule(DCG, Rule)
	,once(prolog_dcg(Rule,DCG)).

test(prolog_dcg_nonterminals, []):-
	DCG = (nonterminals --> n_1, n_2, n_3, n_4)
	,dcg_translate_rule(DCG, Rule)
	,once(prolog_dcg(Rule,DCG)).

test(prolog_dcg_single_terminal_single_nonterminal_, []):-
	DCG = (terminal_nonterminal --> [t], n) % one-one
	,dcg_translate_rule(DCG, Rule)
	,once(prolog_dcg(Rule,DCG)).

test(prolog_dcg_terminals_single_nonterminal, []):-
	DCG = (terminals_nonterminal --> [t_1,t_2,t_3], n) % many - one
	,dcg_translate_rule(DCG, Rule)
	,once(prolog_dcg(Rule,DCG)).

test(prolog_dcg_terminals_nonterminals, []):- % many - many
	DCG = (terminals_nonterminals --> [t_1,t_2,t_3], n_1, n_2)
	,dcg_translate_rule(DCG, Rule)
	,once(prolog_dcg(Rule,DCG)).

test(prolog_dcg_single_terminal_nonterminals, []):- % one - many
	DCG = (terminal_nonterminals --> [t], n_1, n_2)
	,dcg_translate_rule(DCG, Rule)
	,once(prolog_dcg(Rule,DCG)).

test(prolog_dcg_single_nonterminal_single_terminal, []):-
	DCG = (nonterminal_terminal --> n, [t]) % one - one
	,dcg_translate_rule(DCG, Rule)
	,once(prolog_dcg(Rule,DCG)).

test(prolog_dcg_nonterminals_single_terminal, []):- % many - one
	DCG = (nonterminal_terminal --> n_1, n_2, n_3, n_4, [t])
	,dcg_translate_rule(DCG, Rule)
	,once(prolog_dcg(Rule,DCG)).

test(prolog_dcg_nonterminals_single_terminal, []):- % many - many
	DCG = (nonterminal_terminal --> n_1, n_2, n_3, [t_1,t_2,t_3,t_4])
	,dcg_translate_rule(DCG, Rule)
	,once(prolog_dcg(Rule,DCG)).

test(prolog_dcg_nonterminals_single_terminal, []):- % one - many
	DCG = (nonterminal_terminal --> n, [t_1,t_2,t_3])
	,dcg_translate_rule(DCG, Rule)
	,once(prolog_dcg(Rule,DCG)).

% These are kinda random- sure there's a good way to generate series of
% examples.

test(prolog_dcg_mixed_1, []):-
	DCG = (nonterminal_terminal --> n_1, [t_1,t_2], n_2, n_3, [t_3])
	,dcg_translate_rule(DCG, Rule)
	,once(prolog_dcg(Rule,DCG)).

test(prolog_dcg_mixed_2, []):-
	DCG = (nonterminal_terminal --> n_1, n_2,[t_1,t_2],n_3, n_4,[t_3])
	,dcg_translate_rule(DCG, Rule)
	,once(prolog_dcg(Rule,DCG)).

test(prolog_dcg_mixed_3, []):-
	DCG = (nonterminal_terminal --> n_1, [t_1], n_2, [t_2,t_3], n_3)
	,dcg_translate_rule(DCG, Rule)
	,once(prolog_dcg(Rule,DCG)).

test(prolog_dcg_mixed_4, []):-
	DCG = (nonterminal_terminal -->
	      n_1,n_2,[t_1],n_3,n_4,n_5,[t_2,t_3,t_4],n_6,n_7)
	,dcg_translate_rule(DCG, Rule)
	,once(prolog_dcg(Rule,DCG)).

test(prolog_dcg_mixed_5, []):-
	DCG = (nonterminal_terminal -->
	      [t_1,t_2,t_3],n_1,n_2,n_3,[t_4,t_5,t_6],n_4,n_5)
	,dcg_translate_rule(DCG, Rule)
	,once(prolog_dcg(Rule,DCG)).

test(prolog_dcg_mixed_6, []):-
	DCG = (nonterminal_terminal -->
	      [t_1],n_1,n_2,n_3,[t_1,t_2,t_3],n_4,n_5,[t_4,t_5])
	,dcg_translate_rule(DCG, Rule)
	,once(prolog_dcg(Rule,DCG)).

test(prolog_dcg_mixed_7, []):-
	DCG = (nonterminal_terminal --> [t_1],n_1,[t_2],n_2,[t_3])
	,dcg_translate_rule(DCG, Rule)
	,once(prolog_dcg(Rule,DCG)).

test(prolog_dcg_mixed_8, []):-
	DCG = (nonterminal_terminal --> [t_1],n_1,n_2,[t_2,t_3],n_3,[t_4])
	,dcg_translate_rule(DCG, Rule)
	,once(prolog_dcg(Rule,DCG)).

:-end_tests(prolog_dcg).











