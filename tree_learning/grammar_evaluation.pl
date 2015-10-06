:-module(grammar_evaluation, [grammar_evaluation/0]).

:-use_module(configuration).

/** <module> Predicates for evaluating the performance of induced grammars.

*/

%!	metrics_format(?Protocol,?Format) is det.
%
%	The format atom to be passed to format/2 when reporting
%	precision and recall when the value of testing_protocol/1
%	is equal to Protocol.
%
%	Current Formats by Protocol
%	===========================
%
%	precision_recall_bare_bones:
%
%	~w~t~10+   [distribute the first word evenly over 10 chars: the
%		    legnth of the word "Precision" plus a colon; pad
%		    difference with spaces]
%	~w~t~13+   [distribute over 13 chars for "Undetermined" and pad
%	            with spaces].
%	~w~t~2+    [distribute over 2 chars for "on"]
%	~`0t~d~6+  [distribute over 6 digits padding with 0's]
%	~w~t~8|    [distribute over 8 chars for "examples" setting
%		    absolute tab stop at the end]
%
%	precision_recall:
%
%	@TODO: You know what to do.
%
metrics_format(precision_recall_bare_bones, '~w~t~10+ ~w~t~13+ ~w~t~2+ ~`0t~d~6+ ~w~t~8|~n').
metrics_format(precision_recall, '~w~t~10+ ~`0t~d~7+ ~w~t~10+ ~w~t~7+ ~`0t~d~7+ ~w~t~8|~n').
metrics_format(precision_recall_with_reporting, '~w~`.t~15+ ~`0t~d~7+ ~w~t~9+ ~`0t~d~7+ ~w~t~13+ ~w~t~6+  ~`0t~d~7+ ~w~t~18|~n').
metrics_format(precision_recall_with_reporting_separator_recall, '~w~*t~81|~n').
metrics_format(precision_recall_with_reporting_separator_precision, '~w~*t~86|~n').
%	,format(F,['Recall:',Ps,parsed,Us,unparsed,'out of',C,'total examples.'])

%!	format_character(?Format, ?Character) is det.
%
%	Character to use as padding or separator with the given
%	metrics Format.
%
format_character(precision_recall_with_reporting_separator, =).

%!	grammar_evaluation_inference_limit(?Limit) is det.
%
%	The number of inferences for each solution of each separate
%	grammar_evaluation/1 step (eg, for precision_test/2) to attempt
%	before giving up and reporting that the grammar is probably
%	recursive.
%
grammar_evaluation_inference_limit(10_000_000).



%!	grammar_evaluation is det.
%
%	Report on the quality of the grammar using the currently
%	configured testing_protocol/1.
%
grammar_evaluation:-
	configuration:testing_protocol(P)
	,load_examples_module(Ex)
	,load_output_module(Out)
	,metrics_format(P, F)
	,grammar_evaluation(P, Ex, Out, F).


%!	grammar_evaluation(+Testing_protocol) is det.
%
%	Business end of grammar_evaluation/0. Clauses are selected
%	depending on the currently configured testing_protocol/1 option.
%
grammar_evaluation(precision_recall_bare_bones, Ex, Out, F):-
	% TODO: Maybe refactor this, dunno. Could push the format/2 calls to
	%  precision_test or recall_test, or bind its second argument to their
	% "return" variable.
	% Test recall
	examples_count(C)
	,recall_test(precision_recall_bare_bones, Ex,Out,Recall)
	,format(F,['Recall:',Recall,on,C,examples])
	% Test precision
	,precision_test(precision_recall_bare_bones, Ex, Out, Precision)
	,format(F,['Precision:',Precision,on,C,examples])
	,! % Red cut- because I don't know what it's cutting :P
	.

grammar_evaluation(precision_recall, Ex, Out, F):-
	examples_count(C)
	% Test recall
	,recall_test(precision_recall, Ex,Out,Parsed)
	,format(F,['Recall:',Parsed,parsed,'out of',C,examples])
	% Test precision
	,precision_test(precision_recall, Ex, Out, Generated)
	% KLUDGE: If Generated is bound to -1, it will still be padded with 0's
	,format(F,['Precision:',Generated,generated,from,C,examples])
	,! % Red cut- because I still don't know what it's cutting
	.

grammar_evaluation(precision_recall_with_reporting, Ex, Out, F):-
	configuration:output_file_name(grammar_evaluation, S)
	,metrics_format(precision_recall_with_reporting_separator_recall, Sep_rec)
	,metrics_format(precision_recall_with_reporting_separator_precision, Sep_prec)
	,format_character(precision_recall_with_reporting_separator, Sep_char)
	,examples_count(C)
	% Test recall
	,recall_test(precision_recall_with_reporting, Ex,Out, Parsed-Unparsed)
	,length(Parsed, Ps)
	,length(Unparsed, Us)
	,format(S, F,['Recall:',Ps,parsed,Us,unparsed,'out of',C,'total examples.'])
	,atom_codes(Sep_char, [Sep_code])
	,format(S, Sep_rec, [Sep_char,Sep_code])
	,forall(member(P, Parsed), writeln(S, parsed:P))
	,forall(member(U, Unparsed), writeln(S, unparsed:U))
	% Test precision
	,writeln(S, '')
	,precision_test(precision_recall_with_reporting, Ex, Out, In_corpus-Not_in_corpus)
	,length(In_corpus, Ins)
	,length(Not_in_corpus, Nots)
	,Ds is Ins + Nots
	,format(S, F,['Precision:',Ins,'in corpus',Nots,'not in corpus','out of',Ds,'total derivations.'])
	,format(S, Sep_prec, [Sep_char,Sep_code])
	,forall(member(In, In_corpus), writeln(S, 'in corpus':In))
	,forall(member(Not, Not_in_corpus), writeln(S, 'not in corpus':Not)).


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
	configuration:output_file_name(grammar, Output_stream)
	,phrase(configuration:start, [St])
%	,Start_term =.. [/,St,2]
        % "St/2 as St" Reported as error in XPCE- but valid Prolog
	% If that changes use Start_term above.
	,reexport(Output_stream ,except([St/2 as St]) )
	,Output_stream =.. [_Output_file_search_path|[File_name]]
	,file_name_extension(Module_name,_Ext,File_name).



%!	recall_test(+Testing_protocol,+Examples_module,+Grammar_module,-Recall) is det.
%
%	Test the recall of the model.
%
%	Clauses are selected depending on Testing_protocol, the value of
%	configuration setting testing_protocol/1.
%
%	Protocol is one of:
%	* precision_recall_bare_bones.
%	* precision_recall
%
%	With option precision_recall_bare_bones Recall is bound to the
%	atom "total" iff the Grammar can parse each example in the
%	training corpus. Otherwise, Recall is bound to "partial", or
%	"undetermined" if parsing could not complete within N
%	inferences, where N the value of
%	grammar_evaluation_inference_limit/1
%
recall_test(precision_recall_bare_bones, Ex, Out, Recall):-
	findall(S, Ex:example_string(S), Ss)
	,phrase(configuration:start, [St])
	,grammar_evaluation_inference_limit(L)
	,Goal = forall(member(S, Ss), phrase(Out:St, S))
	,call_with_inference_limit(Goal,L,Result)
	,(   Result \= inference_limit_exceeded % probably left-recursion.
	 ->  Recall = total
	 ;   Recall = undetermined
	 ).
recall_test(precision_recall_bare_bones, _Ex, _Out, partial).

recall_test(precision_recall, Ex, Out, Ps_L):-
	findall(S, Ex:example_string(S), Ss)
	,phrase(configuration:start, [St])
	,grammar_evaluation_inference_limit(L)
	% Seems phrase/2 can backtrack over bindings of S and find duplicate parses.
	% Hence the use of setof/3 rather than findall/2
	,Goal = setof(S, (member(S, Ss), phrase(Out:St, S)), Ps)
	,call_with_inference_limit(Goal, L, _Result)
%	,length(Ss, Ss_L)
	,length(Ps, Ps_L).

recall_test(precision_recall_with_reporting, Ex, Out, Parsed-Unparsed):-
	phrase(configuration:start, [St])
	,findall(S, Ex:example_string(S), Ss)
	,once(parsed_unparsed(Out, St, Ss, Parsed, Unparsed)).



%!	precision_test(+Protocol,+Examples_module,+Grammar_module,-Recall) is det.
%
%	Test the precision of the model.
%
%	Clauses are selected depending on the value of configuration
%	setting testing_protocol/1.
%
%	Protocol is one of:
%	* precision_recall_bare_bones
%	* precision_recall
%
%	With precision_recall_bare_bones the atom "total" is bound to
%	Recall iff the Grammar can generate each example in the training
%	corpus. Otherwise Recall is bound to the atom "partial" or to
%	the atom "undetermined" if generation fails to terminate within
%	N inferences, where N the value of
%	grammar_evaluation_inference_limit/1.
%
%	With precision_recall, Recall is bound to the number of strings
%	generated. If generation fails completely, Recall is bound to 0.
%	If the generation goes infinite (or in any case exceeds the
%	specified gammar_evaluation_inference_limit) Recall is bound to
%	-1.
%
precision_test(precision_recall_bare_bones, Ex, Out, Precision):-
	findall(S, Ex:example_string(S), Ss)
	,phrase(configuration:start, [St])
	,grammar_evaluation_inference_limit(L)
	,Goal = forall(phrase(Out:St, S), member(S, Ss))
	,call_with_inference_limit(Goal, L, Result)
	,(   Result \= inference_limit_exceeded
	 ->  Precision = total
	 ;   Precision = undetermined
	 ).
precision_test(precision_recall_bare_bones, _, _, partial).

precision_test(precision_recall, Ex, Out, Ps_L):-
	findall(S, Ex:example_string(S), Ss)
	,phrase(configuration:start, [St])
	,grammar_evaluation_inference_limit(L)
	,Goal = findall(S, (phrase(Out:St, S), member(S, Ss)), Ps)
	,call_with_inference_limit(Goal, L, Result)
%	,length(Ss, Ss_L)
	,(   Result = inference_limit_exceeded
	->   Ps_L = -1
	 ;   length(Ps, Ps_L)
	 ).
precision_test(precision_recall, _, _, 0).

precision_test(precision_recall_with_reporting, Ex, Out, In_corpus-Not_in_corpus):-
	phrase(configuration:start, [St])
	,in_corpus_not_in_corpus(Out, St, Ex, In_corpus, Not_in_corpus).



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



%!	parsed_unparsed(+Grammar,+Production,+Examples,-Parsed,-Unparsed) is det.
%
%	Collect Examples that were Parsed and also the ones that were
%	Unparsed using a Production in the given Grammar.
%
parsed_unparsed(G, P, Es, Ps, Us):-
	parsed_unparsed(G, P, Es, [], Ps_, [], Us_)
	,reverse(Ps_, Ps)
	,reverse(Us_, Us).


%!	parsed_unparsed(+Gramamr,+Production,+Examples,+Ps,-Parsed,+Uns,-Unparsed) is det.
%
%	Business end of parsed_unparsed/5.
%
parsed_unparsed(_, _, [], Ps, Ps, Us, Us).

parsed_unparsed(G, P, [Ex|Es], Ps, Acc_Ps, Us, Acc_Us):-
	once(phrase(G:P, Ex))
	,parsed_unparsed(G, P, Es, [Ex|Ps], Acc_Ps, Us, Acc_Us).

parsed_unparsed(G, P, [Ex|Es], Ps, Acc_ps, Us, Acc_us):-
	parsed_unparsed(G, P, Es, Ps, Acc_ps, [Ex|Us], Acc_us).



%!	in_corpus_not_in_corpus(+Grammar,+Production,+Examples,-Ins,-Outs) is det.
%
%	Report what strings generated by Production in Graemmar are in
%	the given Examples corpus and which are not. Ins are "in
%	corpus", Outs are "not in corpus".
%
in_corpus_not_in_corpus(G, P, E, Ins, Outs):-
	grammar_evaluation_inference_limit(L)
	,Goal = setof(D, phrase(G:P, D), Ds)
	,call_with_inference_limit(Goal, L, Result)
	,(   Result = inference_limit_exceeded
	 ->  Ins = unknown
	    ,Outs = unknown
	 ;   once(in_corpus_not_in_corpus(E, Ds, [], Ins_, [], Outs_))
	 )
	,reverse(Ins_, Ins)
	,reverse(Outs_, Outs).


in_corpus_not_in_corpus(_, [], Ins, Ins, Outs, Outs).
in_corpus_not_in_corpus(E, [D|Ds], Ins, Ins_acc, Outs, Outs_acc):-
	E:example_string(D)
	,in_corpus_not_in_corpus(E, Ds, [D|Ins], Ins_acc, Outs, Outs_acc).
in_corpus_not_in_corpus(E, [D|Ds], Ins, Ins_acc, Outs, Outs_acc):-
	in_corpus_not_in_corpus(E, Ds, Ins, Ins_acc, [D|Outs], Outs_acc).



/*
% Nice but no cigar. We only ever get a single result back, inference limit or not.
%
precision_test(precision_recall, Ex, Out, Ps_L):-
	findall(S, Ex:example_string(S), Ss)
	,phrase(configuration:start, [St])
	,grammar_evaluation_inference_limit(L)
	,grammar_evaluation_step_count(C)
	% Kind of iterative deepening- if the grammar is left recursive
	% we'd still like to get some idea of how many strings were generated
	% before going infinite. So we take C steps at a time, with a limit to
	% the number of inferences at each step until we hit infinity.
	,findnsols(count(C)
		  ,Ps
		  ,call_with_inference_limit(findall(S
						    ,(phrase(Out:St, S), member(S, Ss))
						    ,Ps)
					    ,L, _Result
					    )
		  ,Sols)
	,length(Sols, Ps_L).

You'll need this too if you want to fix it:

%!	grammar_evaluation_step_count(?Count) is det.
%
%	The number of solutions to generate at each step when counting
%	successful parsing or generation attempts. In short, this is
%	passed to findnsols/4 to attempt to count at least some results
%	before going infinite when the grammar is left-recursive.
%
grammar_evaluation_step_count(5).

*/
