:-module(examples_mtg_hand_simulation, [language//0
				       ,terminal//0
				       ,terminals//0
				       ,nonterminal//0
				       ,nonterminals//0
				       ,example//0
				       ,example_string/1]).

:-dynamic nonterminal//0.

%!	language// is nondet.
%
%	A high-level specification of a language type.
language --> nonterminal.


%!	terminals// is nondet.
%
%	Zero or more terminals; part of background knowledge of the
%	target language.
terminals --> [].
terminals --> terminal, terminals.

nonterminals --> [].
nonterminals --> nonterminal, nonterminals.

%!	terminal// is nondet.
%
%	A list of terminals; part of bakground knowledge of the target
%	langauge.
%
%	Note how tokens are wrapped in double square brackets: [[]].
%	This is so bounded_derivation/4 adds them as terminals in the
%	body of a new rule.
%
terminal --> [[]].


%!	nonterminal// is nondet.
%
%	Exactly one nonterminal symbol; part of bakground knowledge of the target
%	langauge.
%
%	These are only the names of nonterminals in our partial grammar.
%	They are wrapped in single square brackets, [], so they will be
%	added as nonterminals (without brackets) in the body of a new
%	rule by bounded_derivation/4.
%
nonterminal --> [].


example --> {example_string(S)}, S.

%!	examples_string(?Example) is nondet.
%
%       A set of example strings that we know belong to the language.
example_string([destroy,target,creature]).
example_string([destroy,target,artifact]).
example_string([exile,target,creature]).
