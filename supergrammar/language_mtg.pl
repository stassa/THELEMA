:-module(language_mtg, [language//0
		       ,terminal//0
		       ,terminals//0
		       ,partial_grammar//0
		       ,nonterminal//0
		       ,nonterminals//0
		       ,cardname//0
		       ,burn//0
		       ,target//0]).

:-dynamic nonterminal//0.

%!	language// is nondet.
%
%	A high-level specification of a language type.
language --> nonterminal.
%language --> [].
%language --> nonterminal, nonterminal.
%language --> nonterminal, terminals.
%language --> terminals, nonterminal, terminals.


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
terminal --> [['Lightning Bolt']].
terminal --> [['Fireball']].
terminal --> [[deals]].
terminal --> [[3]].
terminal --> [['X']].
terminal --> [[damage]].
terminal --> [[to]].
terminal --> [[target]].
terminal --> [[creature]].
terminal --> [[or]].
terminal --> [[player]].

terminal --> [[return]].
terminal --> [[target]].
terminal --> [[creature]].
terminal --> [[to]].
terminal --> [[its]].
terminal --> [['owner''s']].
terminal --> [[hand]].
terminal --> [[destroy]].
terminal --> [[target]].
terminal --> [[creature]].
terminal --> [[or]].
terminal --> [[planeswalker]].
terminal --> [[exile]].
terminal --> [[target]].
terminal --> [[creature]].
terminal --> [[destroy]].
terminal --> [[target]].
terminal --> [[artifact]].


partial_grammar --> cardname, burn.


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
nonterminal --> [cardname].
nonterminal --> [burn].
nonterminal --> [target].

%nonterminal --> terminals.

%!	cardname// is nondet.
%
%	A set of already- known productions that belong to the language.
%
%	TODO: these should not be exported directly- add a 'grammar//0'
%	rule to get them instead.
%
cardname --> ['Lightning Bolt'].
cardname --> ['Fireball'].

burn_amount --> [N], {number(N)}.

%!	burn// is nondet.
%
%	A set of already- known productions that belong to the language.
burn --> [deals,3,damage,to], target.
burn --> [deals,'X',damage,to], target.

%!	target// is nondet.
%
%	A set of already- known productions that belong to the language.
%target --> [target,creature,or,player].
target --> [target],type.
target --> [target],type,[or],type.

type --> [player].
type --> object_type.

object_type --> [creature].
