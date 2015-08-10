:-module(language_simple, [language//0
			  ,start//0
			  ,terminal//0
			  ,terminals//0
			  ,nonterminal//0
			  ,nonterminals//0
			  ]).

:-dynamic nonterminal//0.
:-dynamic terminal//0.

% Or: production --> ... etc?
language --> [].
language --> terminals, nonterminal, terminals.

start --> [s].

terminals --> [].
terminals --> terminal, terminals.

terminal --> [].
%terminal --> [[a]].
%terminal --> [[b]].
%terminal --> [[c]].

nonterminal --> [].

%nonterminal --> [g1].
%nonterminal --> [g2].
%nonterminal --> [g3].
%nonterminal --> [g4].

nonterminals --> [].

% A set of already- known productions that belong to the language.
%g1 --> [a].
%g2 --> g1, [b].
%g3 --> [a,b,c].