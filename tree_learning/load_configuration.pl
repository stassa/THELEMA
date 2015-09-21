:-module(load_configuration, [examples_module/1
			     ,language_module/1
			     ,register_world/3]).

/** <module> Load configuration settings from configuration module into the dynamic database.
*/


%!	examples_module(?Examples_module).
%
%	Dynamic term
:- dynamic examples_module/1.

:- dynamic language_module/1.


%!	register_world(+Language_module,+Language_predicates,+Examples_module) is det.
%
%	Reexport Language_module renaming each predicate in the list
%	of Language_predicates to avoid name clashes. Also reexport
%	Examples_module renaming example_string/1 to example_string/1.
%	Hack to convince Swi to load a different module simply by
%	modifying and consulting this file.
%
%	Note: this works in Swi 7.* but I can't guarantee it will work
%	in anything earlier than that.
%
register_world(Language_module, Renamed_predicates, Examples_module):-
	register_language(Language_module, Renamed_predicates)
	,register_examples(Examples_module).



%!	register_language(+Language_module,+Language_predicates) is det.
%
%	Reexport Language_module renaming each predicate in the list
%	of Language_predicates to avoid name clashes.
%
%	Language_predicates should be a list of terms:
%	Predicate_indicator as New_predicate_name
%
%	Where Predicate_indicator is a name/arity term for a DCG rule:
%	Name//Arity
%
%	The new name can be the same as the current name- this is a bit
%	of a hack but the effect is that we can load a different module
%	just by modifying and then consulting this file (rather than
%	having to exit).
%
%	@SEE register_world.
%
register_language(Language_module, Renamed_predicates):-
	reexport(language(Language_module), except(Renamed_predicates))
	,(  language_module(_)
	->  retract(language_module(_))
	;   true
	)
	,assert(language_module(Language_module))
	,! % Green cut
	.



%!	register_examples(+Examples_module) is det.
%
%	Reexport Examples_module renaming example_string/1 to
%	example_string/1. Same hack as for register_language/2.
%
register_examples(Examples_module):-
	reexport(corpus(Examples_module), except([example_string/1 as example_string]))
	,(  examples_module(_)
	->  retract(examples_module(_))
	;   true
	)
	,assert(examples_module(Examples_module))
	,! % Green cut
	.

