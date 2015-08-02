:-module(at_tokenizer, [write_tokenized/3
		       ,stream_tokenizer/2
		       ,user_tokenizer/1]).

:-use_module(myreadln).

/*
Hint: to keep some tokens from being written out, add a list here (like
readln_stop_chars//), check it and skip its contents, eg in
write_tokenized/2 where you do:

(member(Token_list, Token_lists),member(Token,Token_list))

Add a clause:

(member(Token_list,
Token_lists),member(Token,Token_list),\+memberchk(Token, SkipList)


*/

readln_stop_chars --> [_].  % Leave as default.
readln_word_chars --> [`'{}/1234567890'`].

%!	write_tokenized(+Stream_in,+Stream_out,+Options) is det.
%
%	Read Stream_in to a list of tokens and write it to Stream_out.
%	Options:
%	  delimiter(+Delimiter); the character to use to separate tokens
%	  in Stream_out, for example ',' or ' ' etc.
%	  Hint: if Delimiter = "\n" each token is written in a separate
%	  line.
%	  mode(Mode); how to print out the examples, currently, one of:
%	    wall: wall-of-text mode with all tokens in one line
%	    line: line-by-line mode, with each ability in a separate
%	    line.
write_tokenized(Stream_in, Stream_out, [delimiter(Delimiter),mode(wall)]):-
	stream_tokenizer(Stream_in, Token_lists)
	,open(Stream_out,write,Stream,[encoding(utf8)])
	,forall(
	     (member(Token_list, Token_lists),member(Token,Token_list))
	       ,(write(Stream, Token),write(Stream,Delimiter)))
	,close(Stream)
	,!. % Green - don't check next clause.

% Write each ability on a separate line
write_tokenized(Stream_in, Stream_out, [delimiter(Delimiter),mode(lines)]):-
	stream_tokenizer(Stream_in, Token_lists)
	,open(Stream_out,write,Stream,[encoding(utf8)])
	,forall(member([H|Token_list], Token_lists)
	       ,(write(Stream, H) % Write the first token
		,forall(member(Token, Token_list)
			% Write the remaining tokens preceded by Delimiter
		       ,(write(Stream,Delimiter), write(Stream, Token))
		 )
		% Each token list goes in a new line
		,write(Stream,"\n")
		)
	       )
	,close(Stream).


% write_term(S, T,[fullstop(true),nl(true),spacing(next_argument),quoted(true)]).
write_tokenized(Stream_in, Stream_out, [delimiter(Delimiter),mode(prolog)]):-
	stream_tokenizer(Stream_in, Token_lists)
	,open(Stream_out,write,Stream,[encoding(utf8)])
	,forall(member([H|Token_list], Token_lists)
	       ,(write_prolog(Stream, H) % Write the first token
		,forall(member(Token, Token_list)
			% Write the remaining tokens preceded by Delimiter
		       ,(write(Stream,Delimiter), write_prolog(Stream, Token))
		 )
		% Each token list goes in a new line
		,write(Stream,"\n")
		)
	       )
	,close(Stream).


%!	write_prolog(+Stream,+Token) is det.
%
%	Convenience predicate to print Token to Stream in a
%	Prolog-readable manner. Well that's the intention anyway.
%
write_prolog(Stream, Token):-
	write_term(Stream, Token,[fullstop(false)
				 ,nl(false)
				 ,spacing(next_argument)
				 ,quoted(true)
				 ,character_escapes(true)
				 ]).


%!	stream_tokenizer(+Stream,-Tokens) is det.
%
%	Read from the given Stream and bind to a list of tokens.
%	Based on library(readln), which is that old faithful tokenizer
%	from Clocksin & Mellish, with a few tweaks along the way.
stream_tokenizer(Stream, Tokens):-
	open(Stream, read, Stream_handle, [close_on_abort(true)
					  ,encoding(utf8)])
	,read_lines_from_stream(Stream_handle, [], [], Tokens)
	,! %Green cut
	,close(Stream_handle).


%!	user_tokenizer(-Tokens) is det.
%
%	Use to test stream_tokenizer at the listener.
user_tokenizer(Tokens):-
	 phrase(readln_stop_chars, [Stop_chars])
	,phrase(readln_word_chars, [Word_chars])
	,readln(user_input, Tokens, [10], Stop_chars, Word_chars, uppercase).


%!	read_lines_from_stream(+Stream,-EOF,+Temp,-Acc) is det.
%
%	Business end of stream_tokenizer/2. Reads each line from Stream
%	and binds a list of list of tokens obtained from it to the
%	Accumulator. EOF is bound to the atom and_of_file at the end of
%	the stream.
read_lines_from_stream(_, [-1], Senil, Lines):-
	reverse(Senil, Lines).
read_lines_from_stream(_, [end_of_file], Senil, Lines):-
	reverse(Senil, Lines).
read_lines_from_stream(Stream, _, Temp, Acc):-
	% readln(Stream, P, EOF, StopChars, WordChars, Case)
	phrase(readln_stop_chars, [Stop_chars])
	,readln(Stream, Line, EOF, _, Stop_chars, uppercase)
	,read_lines_from_stream(Stream, EOF, [Line|Temp], Acc).










