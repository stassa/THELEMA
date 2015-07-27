:-module(language, []).

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

