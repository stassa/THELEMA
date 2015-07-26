:-module(language_mtg_hand_simulation_lexicalised, [language//0
		       ,start//0
		       ,terminal//0
		       ,terminals//0
		       ,nonterminal//0
		       ,nonterminals//0
		       ]).

:-dynamic nonterminal//0.

%!	language// is nondet.
%
%	A high-level specification of a language type.
language --> nonterminal.

start --> [ability].

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
terminal --> [].


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
nonterminal --> [at_verb(_)].
nonterminal --> [type].
nonterminal --> [target].

% Not lexicalised:
%at_verb --> [destroy].
%at_verb --> [exile].

%at_verb --> verb(destroy).
%at_verb --> verb(exile).

at_verb(destroy) --> [destroy].
at_verb(exile) --> [exile].

target --> [target].

type --> [creature].
type --> [artifact].

