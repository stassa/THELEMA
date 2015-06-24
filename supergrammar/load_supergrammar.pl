
:-use_module(supergrammar).

edit_source:-
	edit('load_supergrammar.pl')
	,edit('utilities.pl')
	,edit('supergrammar.pl')
	,edit('configuration.pl')
	%,edit('examples_simple.pl')
	,edit('examples_mtg.pl')
	,edit('examples_mtg_lexicalized.pl')
	,edit('run_demo.pl')
	%,edit('C:\\Users\\spatsant\\AppData\\Roaming\\SWI-Prolog\\swipl.ini')
	.

:-edit_source.

run_demo:-
	[run_demo].

