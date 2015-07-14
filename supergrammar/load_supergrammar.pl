
%:-use_module(supergrammar).
:-use_module(stochastic_supergrammar).
:-load_test_files([]).

edit_source:-
	edit('load_supergrammar.pl')
	,edit('utilities.pl')
	,edit('stochastic_supergrammar.pl')
	%,edit('stochastic_supergrammar.plt')
	,edit('configuration.pl')
	,edit('examples_simple.pl')
	,edit('language_simple.pl')
	%,edit('examples_mtg_hand_simulation.pl')
	%,edit('examples_mtg.pl')
	%,edit('examples_mtg_lexicalized.pl')
	%,edit('examples_mtg_stochastic.pl')
	%,edit('C:\\Users\\spatsant\\AppData\\Roaming\\SWI-Prolog\\swipl.ini')
	.

:-edit_source.



