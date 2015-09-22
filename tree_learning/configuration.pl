:-module(configuration, [examples_file_name/1
			,language_file_name/1
			,lexicalisation_strategy/1
			,output_stream/1
			,output_type/1
			,output_format/2
			,production_augmentation/1
			,production_composition/1
			,testing_protocol/1]).

/** <module> Configuration settings for THELEMA

*/

%!	examples_file_name(?Name) is det.
%
%	Basename of the examples file to use in induction.
%examples_file_name(examples_mtg_hand_simulation).
examples_file_name(examples_mtg_destroy_short).
%examples_file_name(examples_mtg_all_destroy_one_sentence_per_line).
%examples_file_name(examples_mtg_all_destroy_cleaned).
%examples_file_name(santeria).
%examples_file_name(santeria_full).

%!	language_file_name(?Name) is det.
%
%	Basename of the language file to use as background knowledge in
%	an induction run.
%language_file_name(language_mtg_hand_simulation).
language_file_name(english).

%!	lexicalisation_strategy(?Strategy:atom) is det.
%
%	How to add lexical parameters to derived productions.
%	One of:
%	* none; don't add lexical parameters.
%	* branch; at a branch add the name of the branch-head production
%	  as a lexical parameter
%       * token; always add the next token as a lexical parameter.
%	* tokens; always add all of the next tokens as lexical
%	  parameters.
%
%	Above, "next token" means the first token after the production's
%	synonym token, ie the token that is the literal name of the
%	production.
%
lexicalisation_strategy(branch).


%!	output_stream(?Name) is det.
%
%	Name of the output file for a derived grammar.
output_stream(output(Filename)):-
	examples_file_name(E)
	,language_file_name(L)
	,output_type(T)
	,output_format(T, Ext)
	,atomic_list_concat([E,L],'_',Base_name)
	,atom_concat(Base_name, Ext, Filename).


%!	output_type(?Type) is det.
%
%	What kind of file to print out at the end of an induction run.
%
%	One of:
%	* grammar; print out a full grammar, from the start symbol
%	defined in the configured language file on down. This is a valid
%	Prolog file with grammar rules in DCG format.
%
output_type(grammar).


%!	output_format(?Type, ?Extension) is det.
%
%	The format of the output file to print out. In short, the
%	extension of the file that pertains to the output_type
%	configured.
%
output_format(grammar, '.pl').


%!	production_augmentation(?Type:atom) is det.
%
%	How to augment node-head productions with new tokens. Type is
%	one of:
%
%	* tail; new constituents are added to the end of the right-hand
%	side of a production.
%
production_augmentation(tail).


%!	production_composition(?Type:atom) is det.
%
%	How to create new node-head productions. Type is one of:
%
%	* basic; node-head productions are created as a new production
%	  named after the current node-head Hi and expanding to a
%	  literal Hi as a terminal.
%
production_composition(basic).


%!	testing_protocol(?Protocol) is det.
%
%	The steps to follow to test and evaluate the derived grammar.
%	Protocol is one of:
%
%	* precision_recall_bare_bones; only report whether a) all
%	examples where parsed correctly and b) all parses are
%	also examples.
%
%
testing_protocol(precision_recall_bare_bones).

/*
 * Loads language and examples files.
 * Don't mess with this bit unless you know what you're doing (tm)
*/
:-examples_file_name(E)
,language_file_name(L)
,register_world(L
	       ,[start//0 as start
		,terminal//0 as terminal
		,nonterminal//0 as nonterminal
		]
	       ,E).
















