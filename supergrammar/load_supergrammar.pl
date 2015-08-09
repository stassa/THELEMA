:-prolog_load_context(directory, Dir)
  ,asserta(user:file_search_path(supergrammar, Dir)).

:- absolute_file_name(.., Dir)
  ,asserta(user:file_search_path(project_root, Dir)).

user:file_search_path(learning_productions, project_root(learning_productions)).


:-use_module(supergrammar).
:-load_test_files([]).

edit_source:-
	edit(supergrammar(load_supergrammar))
	,edit(project_root(utilities))
	,edit(supergrammar(supergrammar))
	,edit(supergrammar(configuration))
	,edit(supergrammar(examples_simple))
	,edit(supergrammar(examples_mtg_hand_simulation))
	,edit(supergrammar(examples_mtg))
	,edit(supergrammar(examples_mtg_lexicalized))
	,edit(supergrammar(examples_mtg_stochastic))
	%,edit(learning_productions(output/language_simple_examples_simple_parsed))
	,edit(supergrammar(run_demo))
	%,edit('C:\\Users\\spatsant\\AppData\\Roaming\\SWI-Prolog\\swipl.ini')
	.

:-edit_source.

run_demo:-
	[run_demo].



