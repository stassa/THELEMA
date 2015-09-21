:-module(grammar_evaluation, [grammar_evaluation/0]).

:-use_module(configuration).

/** <module> Predicates for evaluating the performance of induced grammars.

*/

%!	precision_recall_bare_bones_format(?Format) is det.
%
%	The format atom to be passed to format/2 when reporting
%	precision and recall with precision_recall_bare_bones option
%	configured.
%
%	Current format bits:
%	~w~t~10+   [distribute the first word evenly over 10 chars: the
%		    legnth of the word "Precision" plus a colon; pad
%		    difference with spaces]
%	~w~t~8+    [distribute over 8 chars for "Partial" but pad with
%		    spaces].
%
%	~w~t~2+    [distribute over 2 chars for "on"]
%	~`0t~d~6+  [distribute over 6 digits padding with 0's]
%	~w~t~8|    [distribute over 8 chars for "examples" setting
%		    absolute tab stop at the end]
%
precision_recall_bare_bones_format('~w~t~10+ ~w~t~8+ ~w~t~2+ ~`0t~d~6+ ~w~t~8|~n').

%!	grammar_evaluation_inference_limit(?Limit) is det.
%
%	The number of inferences for each solution of each separate
%	grammar_evaluation/1 step (eg, for precision_test/2) to attempt
%	before giving up and reporting that the grammar is probably
%	left-recursive.
%
grammar_evaluation_inference_limit(100_000).



%!	grammar_evaluation is det.
%
%	Report on the quality of the grammar using the currently
%	configured testing_protocol/1.
%
grammar_evaluation:-
	configuration:testing_protocol(P)
	,grammar_evaluation(P).

%!	grammar_evaluation(+Testing_protocol) is det.
%
%	Business end of grammar_evaluation/0. Clauses are selected
%	depending on the currently configured testing_protocol/1 option.
%
grammar_evaluation(precision_recall_bare_bones):-
	load_examples_module(Ex)
	,examples_count(C)
	,load_output_module(Out)
	,grammar_evaluation_inference_limit(L)
	,precision_recall_bare_bones_format(F)
	% Test recall
	,(   call_with_inference_limit(recall_test(Ex,Out),L,Res_rec)
	    ,Res_rec \= inference_limit_exceeded
	->   Recall = total
	 ;   Recall = partial
	 )
	,format(F,['Recall:',Recall,on,C,examples])
	% Test precision
	,(   call_with_inference_limit(precision_test(Ex, Out),L,Res_pres)
	    ,Res_pres \= inference_limit_exceeded
	 ->  Precision = total
	 ;   Precision = partial
	 )
	,format(F,['Precision:',Precision,on,C,examples])
	% If inference limit was exceeded when attempting to generate examples
	% the grammar is probably left-recursive; report this.
	,(   Res_pres == inference_limit_exceeded
	 ->  writeln('The grammar is probably left-recursive')
	 ;   true
	 )
	,! % Red cut- because I don't know what it's cutting :P
	.



%!	load_examples_module(-Module_name) is det.
%
%	Make sure the examples module is (still) loaded and report its
%	name.
%
load_examples_module(Ex):-
	configuration:examples_module(Ex)
	% Avoid import/1 errors when attempting to load the same start symbol
	% from a different examples module.
	,reexport(corpus(Ex), except([example_string/1 as example_string])).



%!	load_output_module(-Module_name) is det.
%
%	Make sure the output grammar module is loaded and report its
%	name.
%
load_output_module(Module_name):-
	configuration:output_stream(Output_stream)
	,phrase(configuration:start, [St])
%	,Start_term =.. [/,St,2]
        % "St/2 as St" Reported as error in XPCE- but valid Prolog
	% If that changes use Start_term above.
	,reexport(Output_stream ,except([St/2 as St]) )
	,Output_stream =.. [_Output_file_search_path|[File_name]]
	,file_name_extension(Module_name,_Ext,File_name).



%!	recall_test(+Examples_module,+Grammar_module) is det.
%
%	True iff the given Grammar_module can parse each example in the
%	corpus stored in example_string/1 clauses in Examples_module.
%
recall_test(Ex, Out):-
	findall(S, Ex:example_string(S), Ss)
	,phrase(configuration:start, [St])
	,forall(member(S, Ss), phrase(Out:St, S)).



%!	precision_test(+Examples_module,+Grammar_module) is det.
%
%	True iff the given grammar module can generate each example in
%	the corpus stored as example_string/1 clauses in
%	Examples_module.
%
precision_test(Ex, Out):-
	findall(S, Ex:example_string(S), Ss)
	,phrase(configuration:start, [St])
	,forall(phrase(Out:St, S), member(S, Ss)).



%!	examples_count(-Count) is det.
%
%	Count the number of examples in the currently configured
%	examples module(s).
%
%	@TODO: this currently only counts examples in the first
%	configured module. Implement the "(s)" (thought you'll have to
%	do that application-wide, nothing else can handle mulitple
%	examples modules right now.
%
examples_count(Count):-
	configuration:examples_module(Ex)
	% Only inspect examples from the currently configured examples module.
	,findall(S, Ex:example_string(S), Ss)
	,length(Ss, Count).







