
:-use_module(supergrammar).

edit_source:-
	edit('load_supergrammar.pl')
	,edit('utilities.pl')
	,edit('supergrammar.pl')
	,edit('configuration.pl')
	,edit('examples_simple.pl')
	,edit('examples_mtg.pl')
	,edit('examples_mtg_lexicalized.pl')
	,edit('run_demo.pl').

run_demo:-
	[run_demo].

/*
:-edit('load_supergrammar.pl').
:-edit('utilities.pl').
:-edit('supergrammar.pl').
:-edit('configuration.pl').
:-edit('examples_simple.pl').
%:-edit('examples_mtg.pl').
:-edit('examples_mtg_lexicalized.pl').
:-edit('run_demo.pl').
*/
