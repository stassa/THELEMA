:-prolog_load_context(directory, Dir)
  ,writeln(dir:Dir)
  ,asserta(user:file_search_path(supergrammar, Dir)).

user:file_search_path(examples, supergrammar(examples)).
% Directory for examples and language modules
user:file_search_path(language, supergrammar(languages)).

:-nodebug(update_augmentation_set).
:-debug(new_production).
:-nodebug(update_grammar).
:-nodebug(prune_corpus).
:-nodebug(augment_production).
:-nodebug(score_production).


% Loads configuration predicates into global module user where
% configuration module can access them.
:-use_module(load_configuration).
%:-use_module(supergrammar).
:-use_module(stochastic_supergrammar).

:-load_test_files([]).

%!	edit_source is det.
%
%	Opens work files in the editor.

edit_source:-
	edit(supergrammar(load_supergrammar))
	,edit(supergrammar(utilities))
	,edit(supergrammar(stochastic_supergrammar))
	,edit(supergrammar('stochastic_supergrammar.plt'))
	,edit(supergrammar(configuration))
	,edit(supergrammar(load_configuration))
	% Load configured language and examples files :D
	,configuration:language_module(Language)
	,configuration:examples_module(Examples)
	,edit(language(Language))
	,edit(language(Examples))

	% Might still need this
	%,edit('C:\\Users\\spatsant\\AppData\\Roaming\\SWI-Prolog\\swipl.ini')
	.

:-edit_source.



