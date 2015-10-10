:-module(configuration, [examples_file_name/1
%			,grammar_printing/1
			,language_file_name/1
			,lexicalisation_strategy/1
			,output_file_name/2
			,output_type/1
			,output_format/2
			,production_augmentation/1
			,production_composition/1
			,rename_built_ins/1
			,remember_previous_results/1
			,testing_protocol/1
			]).

/** <module> Configuration settings for THELEMA

*/

%!	examples_file_name(?Name) is det.
%
%	Basename of the examples file to use in induction.
%examples_file_name(examples_mtg_hand_simulation).
examples_file_name(mtg_pot_puri).
%examples_file_name(examples_mtg_destroy_short).
%examples_file_name(examples_mtg_all_destroy_one_sentence_per_line).
%examples_file_name(examples_mtg_all_destroy_cleaned).
%examples_file_name(mtg_all_destroy).

%examples_file_name(examples_mtg_hand_simulation_compressed).
%examples_file_name(examples_mtg_destroy_short_compressed).

%examples_file_name(santeria).
%examples_file_name(santeria_full).


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
%	* greibach; as token/s but the next token is always a
%	nonterminal. This is just to clarify that lexicalisation will
%	follow GNF conventions.
%
%	Above, "next token" means the first token after the production's
%	synonym token, ie the token that is the literal name of the
%	production.
%
lexicalisation_strategy(greibach).


%!	output_stream(?Type,?Name) is det.
%
%	Name of the output file for a derived grammar or a compressed
%	corpus. Type selects clauses according to the type of file to
%	print.
%
output_file_name(grammar, output(Filename)):-
	%(   Type = grammar
	%;   Type = bnf
	%;   Type = ebnf
	%;   Type = hex_bnf
	%)
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

output_file_name(compressed_corpus, corpus(Filename)):-
	examples_file_name(E)
	% Output type/format shared with grammar!
	,output_type(T)
	,output_format(T, Ext)
	,atomic_list_concat([E,compressed],'_',Base_name)
	,atom_concat(Base_name, Ext, Filename).

% Use to print to Prolog top-level
output_file_name(grammar_evaluation, user_output).
/*output_file_name(grammar_evaluation, output(Filename)):-
	output_file_name(grammar, output(Grammar))
	,output_format(evaluation, Ext)
	,atom_concat(Grammar, Ext, Filename).
*/


%!	output_type(?Type) is det.
%
%	What kind of file to print out at the end of an induction run.
%
%	One of:
%	* grammar; print out a full grammar with every production
%	connected to the start symbol, or to a production connected to
%	the start symbol. The output file is a valid Prolog file with
%	grammar rules in DCG format.
%	* tags; print a set of productions that may be used to carve
%	up a sentence into parts-of-speech, or indeed comrpess it.
%	* tree; as "grammar" but with a parameterised start-symbol that
%	captures the parse tree of a parsed (or generated) phrase.
%	* compression; print a compression grammar, used to replace
%	examples' tokens with the names of productions that cover them;
%	implies "tags" (and indeed includes the tags grammar).
%	* bnf; as "grammar" but the output is in Backus-Naur Form.
%	* hex_bnf; as bnf, but names of nonterminals are output in
%	hexadecimal codes for use with tools that conform to xml specs
%	(bleagh) for example Railroad Diagram Generator.
%	* ebnf; prints grammar in extended bnf.
%	* dot; print a dot-language file that can be used to generate a
%	visualisation of the grammar using a program such as graphviz.
%	* lean_dot; as dot, but only prints nonterminal and
%	preterminal nodes (dot normally prints terminals also, like in
%	a parse tree).
%
%	@TODO: implement dot.
%
output_type(lean_dot).


%!	output_format(?Type, ?Extension) is det.
%
%	The format of the output file to print out. In short, the
%	extension of the file that pertains to the output_type
%	configured.
%
output_format(grammar, '.pl').
output_format(tags, '.pl').
output_format(compression, '.pl').
output_format(bnf, '.bnf').
output_format(hex_bnf, '.hbnf').
output_format(ebnf, '.ebnf').
output_format(dot, '.gv').
output_format(lean_dot, '.gv').
%output_format(evaluation, '.log').


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
production_composition(synonym).


%!	remember_previous_results(?Bool) is det.
%
%	Whether, when encountering a new branch-head token, to look for
%	previously derived productions covering that token, or not.
%
%	Option "true" will also turn on the use of the dynamic database
%	to record such previously obtained results for reuse.
%
%	Bool is one of 'true' or 'false' where duh.
%
remember_previous_results(true).


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
%	* basic; only report whether a) allexamples where parsed
%	correctly and b) all parses are also examples.
%	* counts; report the number of parsed strings over total number
%	of examples and the number of generated strings over total
%	number of examples.
%	* strings; print out the parsed and unparsed examples as well as
%	the generated strings that were in and not in the corpus.
%
testing_protocol(strings).


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
















