:-module(examples_simple, [language//0
			  ,terminal//0
			  ,terminals//0
			  ,nonterminal//0
			  ,nonterminals//0
			  ,example_string/1
			  ,g1//0
			  ,g2//0
			  ,g3//0
			  ]).

:-dynamic nonterminal//0.

% Or: production --> ... etc?
language --> [].
language --> terminals, nonterminal, terminals.

terminals --> [].
terminals --> terminal, terminals.

terminal --> [[a]].
terminal --> [[b]].
terminal --> [[c]].

nonterminal --> [g1].
nonterminal --> [g2].
nonterminal --> [g3].
%nonterminal --> [g4].

nonterminals --> [].

% A set of already- known productions that belong to the language.
g1 --> [a].
g2 --> g1, [b].
g3 --> [a,b,c].

% A set of example strings that we know belong to the language
example_string([a]).
example_string([a,b]).
example_string([a,b,c]).
