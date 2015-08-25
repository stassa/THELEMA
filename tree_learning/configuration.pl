:-module(configuration, [examples_file_name/1
			,language_file_name/1
			,output_stream/1
			,output_type/1
			,output_format/2]).


%!	examples_file_name(?Name) is det.
%
%	Basename of the examples file to use in induction.
%examples_file_name(examples_mtg_hand_simulation).
examples_file_name(examples_mtg_destroy_short).
%examples_file_name(examples_mtg_all_destroy_one_sentence_per_line).




%!	language_file_name(?Name) is det.
%
%	Basename of the language file to use as background knowledge in
%	an induction run.
language_file_name(language_mtg_hand_simulation).


:-examples_file_name(E)
,language_file_name(L)
,register_world(L
	       ,[start//0 as start
		,terminal//0 as terminal
		,nonterminal//0 as nonterminal
		]
	       ,E).


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