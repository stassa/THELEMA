:-module(language, [start//0
		   ,terminals//0
		   ,nonterminals//0
		   ,terminal//0
		   ,nonterminal//0
		   ,e//0]).

:-dynamic start//0.
:-dynamic nonterminal//0.
:-dynamic terminal//0.

start --> [start_symbol].

start_symbol --> terminals.
start_symbol --> nonterminals.

terminals --> e.
terminals --> terminal, terminals.

nonterminals --> e.
nonterminals --> nonterminal, nonterminals.

terminal --> e.

nonterminal --> e.

e --> [].

