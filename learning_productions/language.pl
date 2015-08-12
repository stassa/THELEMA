:-module(language, [start//0
		   ,terminals//0
		   ,nonterminals//0
		   ,terminal//0
		   ,nonterminal//0
		   ,e//0]).

% Imported to allow it to see productions in language modules.
:-use_module(project_root(utilities), [productions_compressed_strings/3]).

:-dynamic start//0.
:-dynamic nonterminal//0.
:-dynamic terminal//0.

% Call start//0 in the context of language modules inheriting from
% this module (allowing it to be overriden in those modules).
:-module_transparent start//0.

%!	start// is det.
%
%	The start symbol of the grammar.
start --> [S], { phrase(start_symbol, [S]) }.

%!	start_symbol// is det.
%
%	The start symbol of the grammar.
%
%	Convenience predicate meant to be overriden by language modules
%	importing this module. The idea is that each inheriting module
%	can declare its own start symbol in a start_symbol//0 clause
%	and still export start//0 therefore conforming to the
%	specification of a language module declared in this module.
%
%	Yeah, convoluted rather. "A little bit of the OOP world came
%	through and complicated him to death".
%
start_symbol --> [s].

%!	terminals// is nondet.
%
%	Zero or more terminals; part of background knowledge of the
%	target language.
terminals --> e.
terminals --> terminal, terminals.

%!	terminals// is nondet.
%
%	Zero or more nonterminals.
nonterminals --> e.
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
terminal --> e.

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
nonterminal --> e.

e --> [].

