:-module(grammar_printing, [print_productions/0
			   ,print_grammar/0]).

:-use_module(production_induction, [corpus_productions/2]).


%!	print_corpus_productions is det.
%
%	Derive and print (to standard out) grammar productions learned
%	from the currently configured examples and language modules.
%
print_productions:-
	configuration:examples_module(Es)
	,configuration:language_module(L)
	,phrase(L:start, [S])
	,findall([S|C], Es:example_string(C), Cs)
	,corpus_productions(Cs, Ps)
	,!
	,forall(member(P, Ps), writeln(P)).



%!	print_grammar is det.
%
%	Derive and print to the configured output file the grammar
%	learned from the currently configured etc.
%
print_grammar:-
	configuration:examples_module(Es)
	,configuration:language_module(L)
	,phrase(L:start, [S])
	,findall([S|C], Es:example_string(C), Cs)
	,corpus_productions(Cs, Ps)
	,!
	,configuration:output_stream(O)
	,expand_file_search_path(O, P)
	,open(P,write,Stream,[])
	,once(print_grammar_file(Stream, S, Ps))
	,close(Stream)
	,edit(P).


print_grammar_file(Stream, S, Ps):-
	stream_property(Stream, file_name(Path))
	,file_base_name(Path, Filename)
	,file_name_extension(Grammar_module_name, _Ext, Filename)
	,format(Stream, '~w~w~w~w~w~n'
	       ,[':-module(',Grammar_module_name,',',[S//0],').'])
	,write(Stream, '\n')
	,forall(member(P, Ps), print_term(Stream, p, P)).

print_term(S, p, T):-
	write_term(S, T,[fullstop(true),nl(true),spacing(next_argument),quoted(true)]).




