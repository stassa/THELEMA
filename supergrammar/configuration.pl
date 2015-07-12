:-module(configuration, []).

:- dynamic examples_module/1.

/** <module> Configuration for supergrammar generate-and-test cycles.

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


TODO: Complete this.

 */


% TODO: make this configurable outside Prolog - eg, read in the name of
% the examples module from an xml or other config file.

%!	register_examples(+Module, +Predicates) is det.
%
%	Reexport Module renaming each Predicate to avoid name clashes.
%	Predicates is a list of terms:
%	  Predicate_indicator as New_predicate_name
%
%      The new name can be the same as the current name- this is a bit
%      of a hack but the effect is that we can load a different module
%      just by modifying and then consulting this file (rather than
%      having to exit).
%
%      Note: this works in Swi 7.* but I can't guarantee it will work in
%      anything earlier than that.
%
register_examples(Examples_module, Renamed_predicates):-
	reexport(Examples_module, except(Renamed_predicates))
	,(  examples_module(_)
	->  retract(examples_module(_))
	;   true
	)
	,assert(examples_module(Examples_module))
	,! % Green cut
	.

:-register_examples(examples_simple
%:-register_examples(examples_mtg_lexicalized
%:-register_examples(examples_mtg
%:-register_examples(examples_mtg_hand_simulation
		,[language//0 as language
		 ,terminal//0 as terminal
		 ,terminals//0 as terminals
		 ,nonterminal//0 as nonterminal
		 ,nonterminals//0 as nonterminals
		 ,example_string/1 as example_string
		 ] ).


