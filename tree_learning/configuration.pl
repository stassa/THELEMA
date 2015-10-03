:-module(configuration, [examples_file_name/1
			,grammar_printing/1
			,language_file_name/1
			,lexicalisation_strategy/1
			,output_stream/2
			,output_type/1
			,output_format/2
			,production_augmentation/1
			,production_composition/1
			,rename_built_ins/1
			,testing_protocol/1]).

/** <module> Configuration settings for THELEMA

*/

%!	examples_file_name(?Name) is det.
%
%	Basename of the examples file to use in induction.
examples_file_name(mtg_pot_puri).
%examples_file_name(examples_mtg_hand_simulation).
%examples_file_name(examples_mtg_hand_simulation_compressed).
%examples_file_name(examples_mtg_destroy_short).
%examples_file_name(examples_mtg_destroy_short_compressed).
%examples_file_name(examples_mtg_all_destroy_one_sentence_per_line).
%examples_file_name(examples_mtg_all_destroy_cleaned).
%examples_file_name(santeria).
%examples_file_name(santeria_full).

%!	grammar_printing(?Type) is det.
%
%	What type of grammar to print. One of:
%
%	* tree; print a fully hierarchical grammar with every production
%	connected to the start symbol or to a production connected to
%	the start symbol.
%	* tags; print a set of productions that may be used to carve
%	up a sentence into parts-of-speech, or indeed comrpess it.
%	* compression; print a compression grammar, used to replace
%	examples' tokens with the names of productions that cover them;
%	implies "tags" (and indeed includes the tags grammar).
%
%      @TODO: this is redundant; we got output_type.
%      @KLUDGE: You don't actually need "tags" as a separate option.
%      We print PsOS into the compression grammar anyway. I don't see
%      why we need it as a separate file. Mneh, OK, maybe for other
%      programs...
%
grammar_printing(tree).


%!	language_file_name(?Name) is det.
%
%	Basename of the language file to use as background knowledge in
%	an induction run.
language_file_name(language_mtg_hand_simulation).
%language_file_name(english).

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
lexicalisation_strategy(none).


%!	output_stream(?Type,?Name) is det.
%
%	Name of the output file for a derived grammar or a compressed
%	corpus. Type selects clauses according to the type of file to
%	print.
%
output_stream(grammar, output(Filename)):-
	examples_file_name(E)
	,language_file_name(L)
	,output_type(T)
	,output_format(T, Ext)
	,lexicalisation_strategy(S)
	,(   S \= none
	->  atom_concat('lexicalised_by_', S, S_)
	;   S_ = 'not_lexicalised'
	 )
	,atomic_list_concat([E,L,S_],'_',Base_name)
	,atom_concat(Base_name, Ext, Filename).

output_stream(compressed_corpus, corpus(Filename)):-
	examples_file_name(E)
	% Output type/format shared with grammar!
	,output_type(T)
	,output_format(T, Ext)
	,atomic_list_concat([E,compressed],'_',Base_name)
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
%	* literals; ignore nonterninal references and only keep
%	literals. Only makes sense if grammar_printing/1 is set to tag
%	also.
%	* greibach; always follow a single terminal with a single
%	nonterminal or the empty string, technically leaving the
%	production in Greibach Normal Form (though GNF allows more than
%	one nonterminals).
%
production_augmentation(greibach).


%!	production_composition(?Type:atom) is det.
%
%	How to create new node-head productions. Type is one of:
%
%	* synonym; node-head productions are created as a new
%	production named after the current node-head Hi and expanding to
%	a literal Hi as a terminal; in other words, the production is a
%	synonym of its constituents.
%
%	@TODO: change "basic" to "synonym"
%
production_composition(basic).


%!	rename_built_ins(?Bool_or_prefix) is det.
%
%	Whether to rename nonterminals that will compile to built-ins at
%	the DCG compiler.
%
%	Bool_or_prefix is either the atom "false", in which case
%	built-ins are not renamed or an atom to be used as the prefix of
%	renamed productions.
%
%	Note: if you want the prefix to end in an underscore you have to
%	specify it here, for example:
%	rename_built_ins(n_)
%
%	Will result in productions like:
%	n_is --> [is]...
%
%	Wheras:
%	rename_built_ins(n)
%
%	Will only give you:
%	nis --> [is] ...
%
rename_built_ins(n_).


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
testing_protocol(precision_recall).

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
















