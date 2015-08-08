:-module(language_mtg_lexicalized, [language//0
				   ,start//0
				   ,terminal//0
				   ,terminals//0
				   ,nonterminal//0
				   ,nonterminals//0
				   ,cardname//0
				   ,burn//0
				   ,destroy//0
				   ,exile//0
				   ,bounce//0
				   ,target//1]).

% TODO: this should be in configuration module but then it gives a
% warning (overriding weak import).
:-dynamic
	nonterminal//0.


%!	language// is nondet.
%
%	A high-level specification of a language type.
language --> nonterminal(nn(cardname)), nonterminal(ab(spell_ability)).
language --> nonterminal(ab(spell_ability)).


start --> [ability].


/*
lexical categories:
nn : noun (cardname)
ab: ability  (spell_ability, activated_ability, static_ability)
det: determiner (target)
tp: type
*/

%!	terminals// is nondet.
%
%	Zero or more terminals; part of background knowledge of the
%	target language.
terminals --> [].
terminals --> terminal, terminals.


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

nonterminals --> [].

nonterminal --> nonterminal(_).

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
nonterminal(nn(cardname)) --> [cardname].
nonterminal(ab(spell_ability)) --> [burn].
nonterminal(ab(spell_ability)) --> [destroy].
nonterminal(ab(spell_ability)) --> [exile].
nonterminal(ab(spell_ability)) --> [bounce].
nonterminal(det(target)) --> [target].

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
burn --> [deals,3,damage,to], target(_).
burn --> [deals,'X',damage,to], target(_).

destroy --> [destroy],target(tp(object)).

exile --> [exile], target(tp(object)).

bounce --> [return],target(tp(object)),[to,its,'owner''s',hand].

%!	target// is nondet.
%
%	A set of already- known productions that belong to the language.
target(T) --> [target],type(T).
target(or(T1,T2)) --> [target],type(T1),[or],type(T2)
          ,{T1 \== tp(player) % player always goes after 'creature' or other types
	   ,T1 \== T2}.

type(tp(player)) --> [player].
type(tp(object)) --> object_type.

object_type --> [creature].
object_type --> [artifact].
