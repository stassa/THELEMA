:-prolog_load_context(directory, Dir)
  ,asserta(user:file_search_path(tree_learning, Dir)).

:- absolute_file_name(.., Dir)
  ,asserta(user:file_search_path(project_root, Dir)).

user:file_search_path(corpus, tree_learning(corpus)).
user:file_search_path(language, tree_learning(language)).
user:file_search_path(output, tree_learning(output)).

:-use_module(load_configuration).
:-use_module(language).
:-use_module(production_induction).
:-use_module(grammar_printing).
:-use_module(grammar_evaluation).

%:-use_module(production_induction_v1).
%:-use_module(grammar_printing_v1).


% Uncomment when we have some
%:-load_test_files([]).

edit_source:-
	edit(tree_learning(load_tree_learning))
	,edit(tree_learning(production_induction))
	%,edit(tree_learning(production_induction_v1))
	,edit(tree_learning(grammar_evaluation))
	,edit(tree_learning(grammar_printing))
	%,edit(tree_learning(grammar_printing_v1))
%	,edit(tree_learning(load_configuration))
	,edit(tree_learning(configuration))
%	,edit(tree_learning(language))
%	,configuration:language_module(Language)
	,configuration:examples_module(Examples)
%	,edit(language(Language))
	,edit(corpus(Examples)).
:-edit_source.

set_spy_points:-
	spy([you_are_here
	    ,derived_productions/5
	    ,augmented_node_head_production/3
	    ]).
