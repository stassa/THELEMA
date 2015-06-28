:-module(utilities, [diff_list/3
		    ,diff_append/3
		    ,pdcg_parses/3
		    ,pdcg_parse/2
		    ,max_key/2
		    ,list_tree/2
		    ,tree_list/2
		    ,rule_name/1
		    ,generate_alphanumeric/2
		    ,permute/2]).

/** <module> Utility predicates used in supergrammar module.
*/

%!	diff_list(+List,-Difference_list,?Tail) is nondet.
%
%	Difference_list is List and Tail is its Tail.
%
%	To produce ground list bind Tail to [], as in:
%	  diff_list([a,b,c],Dif,[]).
%
diff_list(L, Diff, Tail):-
	once(phrase(diff_list_0(L), Diff, Tail)).

diff_list_0(L) --> L.

/*
DCG version is actually slightly faster.

diff_list(List, Diff, Tail):-
	tail(List, Tail, Diff).

tail([H], T, [H|T]).
tail([H|T], Temp, Acc):-
	tail(T,	Temp, Acc1)
	,tail([H], Acc1, Acc).
*/



%!	diff_append(?L1,?L2,?L3) is nondet.
%
%	Difference list version of append/3.
%
%	Example usage:
%
%	You can concatenate two difference lists:
%
%	==
%       ?-diff_append([a,b|A]-A,[c,d|B]-B,T-B).
%       A = [c, d|B],
%       T = [a, b, c, d|B].
%       ==
%
%	Or three of them:
%
%       ==
%       ?- diff_append([a,b|A]-A,[c,d|B]-B,T-[e,f|C]).
%       A = [c, d, e, f|C],
%       B = [e, f|C],
%       T = [a, b, c, d, e, f|C].
%       ==
%
%	Or one difference list and one ground list, to obtain a ground
%	list:
%
%       ==
%	?- diff_append([a,b|B]-B, [c,d]-T, A-T).
%	B = [c, d],
%       A = [a, b, c, d].
%       ==
%
%       You can split two lists:
%
%	==
%       ?- utilities:diff_append([a,b|B]-B, B-T, [a,b,c,d|T]-T).
%       B = [c, d|T].
%       ==
%
%	You can ignore the tail of the ground list, or bind it to [],
%	or indeed any term you want to carry around:
%
%       ==
%	?- diff_append([a,b|B]-B, [c,d]-[], A-_).
%	B = [c, d],
%       A = [a, b, c, d].
%
%	?- diff_append([a,b|B]-B, [c,d]-[abc],	A-[abc]).
%       B = [c, d],
%       A = [a, b, c, d].
%       ==
%
%	You can also concatenate trees in the same manner:
%
%	==
%	?- diff_append((a,b,A)-A,(c,d,B)-B,T-B).
%       A = (c, d, B),
%       T = (a, b, c, d, B).
%
%	?- diff_append((a,b,A)-A,(c,d)-B,T-B).
%	A = (c, d),
%       T = (a, b, c, d).
%
%       diff_append((a,b,A)-A,(c,d,B)-B,T-(e,f,C)).
%       A = (c, d, e, f, C),
%       B = (e, f, C),
%       T = (a, b, c, d, e, f, C).
%       ==
%
%       Or any two (or three) terms really:
%
%       ==
%       ?- diff_append(f(a,b,A)-A,g(c,d,B)-B,T-B).
%       A = g(c, d, B),
%       T = f(a, b, g(c, d, B)).
%
%       ?- diff_append(f(a,b,A)-A,g(c,d,B)-B,T-v(e,f,C)).
%       A = g(c, d, v(e, f, C)),
%       B = v(e, f, C),
%       T = f(a, b, g(c, d, v(e, f, C)))
%       ==
%
%	TODO: Move to libraries
%
diff_append(A-B, B-T, A-T).



%!	pdcg_parses(+Production,+Cutoff,-Derivation) is nondet.
%
%	Derivation is a term P-D where P the probability of D and D a
%	derivation of the rewrite rule Production.
%
%	Each possible P-D pair is generated on subsequent backtracking.
pdcg_parses(Rule, Cutoff, P-D):-
	findall(Probability-Parse
	       ,phrase(Rule, Parse, Probability)
	       ,Parses)
	,ground_kvps(Parses, Ground)
	,aggregated_keys(Ground, KVPs)
	,member(P-D, KVPs)
	,P >= Cutoff.



%, or the list of derivations of Production by order
%	of probability.
%

%!	pdcg_parse(+Production,-Derivation) is det.
%
%	True when Derivation is the most probable derivation of
%	Production
%	Derivation is of the form: P-D, where P is the probability of D.
%
pdcg_parse(Rule, Parse):-
	findall(Probability-Parse
		% Hm. This will not work if Probability wound not be []
	       ,phrase(Rule, Parse, Probability)
	       ,Parses)
	,most_likely(Parses, Parse).


%!	most_likely(+Parses, -Parse) is det.
%
%	Parses is a list of key-value pairs of the form:
%
%	[P1,P2, ..., Pn]-[V1,V2, ..., Vn]
%
%	Where P1...Pn is a list of numbers representing the joint (?)
%	probabilities of V1...Vn and each Vi is a token in the
%	derivation V1...Vn.
%
%	True when Parse is the element of Parses with the highest
%	probability.
most_likely(Parses, Parse):-
	ground_kvps(Parses, Gs)
	,aggregated_keys(Gs, Ps)
	,max_key(Ps, Parse).


%!	ground_kvps(+KVPs, -Ground_kvps) is det.
%
%	True when KVPs is a difference list of key-value pairs delimited
%	by '-' and Ground_kvps is the same list with tail-variables
%	removed.
ground_kvps(KVPs, Ground):-
	ground_kvps(KVPs, [], Ground).

%!	ground_kvps(+KVPs,+Temp,-Acc) is det.
%
%	Business end of ground_kvps/2.
ground_kvps([], Gs, Gs).
ground_kvps([K-V|Ls], Temp, Acc):-
	once(phrase(K, Kk, []))
	,once(phrase(V, Vv, []))
	,ground_kvps(Ls, [Kk-Vv|Temp], Acc).


%!	aggregated_keys(+Keys_values_pairs,-Key_value_pairs) is det.
%
%	Convert between a list of key-value pairs where each key is a
%	list of numbers to one where each key is a single number.
aggregated_keys(Kvps, Product):-
	aggregated_keys(Kvps, [], Product).

%!	aggregated_keys(+Keys,+Temp,-Acc) is det.
%
%	Business end of aggregated_keys/2.
aggregated_keys([], Ks, Ks).
aggregated_keys([K-V|KVPs], Temp, Acc):-
	foldl(multiplication, K, 1, P)
	,aggregated_keys(KVPs, [P-V|Temp], Acc).


:-begin_tests(multiplication).

test(multiplication_mode_var_var_ground_1
    ,[throws(error(instantiation_error,_))]):-
	multiplication(_, 2, _).

test(multiplication_mode_var_var_ground_2
    ,[throws(error(instantiation_error,_))]):-
	multiplication(1, _, _).

% Don't use to verify multiplication of two numbers
test(multiplication_mode_var_var_ground_3
    ,[throws(error(uninstantiation_error(2),_))]):-
	multiplication(1, 2, 2).

test(multiplication_three_numbers, []):-
	multiplication(3, 4, R)
	,R = 12.

test(multiplication_alpha_number, []):-
	multiplication(abc, 4, R)
	,R = 4.

test(multiplication_number_alpha, []):-
	multiplication(5, def, R)
	,R = 5.

test(multiplication_two_alphas, []):-
	multiplication(abc, def, R)
	,R = 1.

:-end_tests(multiplication).

%!	multiplication(+A,+B,-C) is det.
%
%	True when C is A * B, given that both A and B are numbers.
%
%	If either A or B is not a number C is bound to the one that is a
%	number.
%
%	If both A and B are not numbers, C is bound to 1.
%
multiplication(A, B, C):-
	must_be(nonvar, A)
	,must_be(nonvar,B)
	,must_be(var, C)
	,fail.
multiplication(A, B, B):-
	\+ number(A)
	,number(B)
	,!.
multiplication(A, B, A):-
	\+ number(B)
	,number(A)
	,!.
multiplication(A, B, 1):-
	 \+ number(A)
	,\+ number(B)
	,!.
multiplication(A, B, C):-
	number(A)
	,number(B)
	,C is A * B.



%!	keymax(+Keys,-Max) is det.
%
%	Max is the kvp with the largest key in Keys.
max_key(Keys, Max):-
	keysort(Keys, Sorted)
	,reverse(Sorted, [Max|_Reversed]).



%!	list_tree(+List, -Tree) is nondet.
%
%	Convert a list to a tree. Doesn't go the other way. Don't try
%	it.
%
%	Warned you.
list_tree(Ls, Nts):-
	reverse(Ls,[L|Rev])
	,list_tree(Rev, L, Nts)
	,!.

%!	list_tree(+Ls,+Temp,-Acc) is nondet.
%
%	Business end of list_tree/2. First clauese handles
%	single-element lists in list_tree.
list_tree([], N, (N)).
list_tree([N], Ns, (N,Ns)).
list_tree([N|Ns], Temp, Acc):-
	list_tree(Ns, (N,Temp), Acc).



%!	tree_list(+Tree,-List) is det.
%
%	Convert between a Tree and a List.
tree_list(Tree, List):-
	Tree =.. Terms_list
	,tree_list(Terms_list, [], List)
	,!.

tree_list(Tree, Diff_list-Tail):-
	Tree =.. Terms_list
	,tree_list(Terms_list, [], List)
	,diff_list(List, Diff_list, Tail).


%!	tree_list(+Terms_list,+Temp,-Acc) is nondet.
%
%	Business end of tree_list/2. Remove functor heads of
%	parenthesised terms and lists from a list of terms obtained by
%	=../2.
tree_list([], Sl, Ls):-
	reverse(Sl, Ls).
tree_list([T|Ts], Temp, Acc):-
	(   T = ','
	;   T = '[|]')
	,!
	,tree_list(Ts, Temp, Acc).
tree_list([T|Ts], Temp, Acc):-
	compound(T) % Nested parens
	,\+ is_list(T) % Leave lists unopened.
	,!
	,T =.. Cs
	,append(Cs, Ts, Tts)
	,tree_list(Tts, Temp, Acc).
tree_list([T|Ts], Temp, Acc):-
	tree_list(Ts, [T|Temp], Acc).



%!	rule_name(?Functor) is nondet.
%
%	True when F conforms to the regular expression:
%	==
%       [a-zA-Z]+[0-9]
%       ==
%	Used to geneate DCG rule functors for productions
%	derived by predicates in module supergrammar.
rule_name(F):-
	nonvar(F)
	,atom_chars(F, H)
	,phrase(letters_number, H)
	,!.
rule_name(F):-
	var(F)
	,phrase(letters_number, H)
	,atomic_list_concat(H,F).


:-begin_tests(generate_alphanumeric).

/* Tests for all-default options */

% Attempting to call in mode (+,+) throws an uninstantiation_error:
% You shoud not use this predicate to verify the type of a list.
test(generate_alphanumeric_used_as_checker, [throws(error(uninstantiation_error([stassa,stassa]),_))]):-
	generate_alphanumeric([], [stassa,stassa]).

% Shows that empty options list should generate a list of length exactly
% two and containing exactly one letter and exactly one number. The
% order is random but that is rather difficult to show :)
test(generate_alphanumeric_default, [nondet]):-
	generate_alphanumeric([], L)
	,is_alnum_set_of_two(L).


/* tests for single non-default options */

% Shows that specifying only option generate(alnums) has the same effect
% as specifying an empty option list (since alnums is default).
test(generate_alphanumeric_generate_alnums, [nondet]):-
	generate_alphanumeric([generate(alnums)], L)
	,is_alnum_set_of_two(L).

% Shows that specifying option generate(alphas) generates a list of
% exactly two alphabetic characters.
test(generate_alphanumeric_generate_alphase, [nondet]):-
	generate_alphanumeric([generate(alphas)], L)
	,is_set_of(L, 2, alpha).

% Test that specifying option generate(alpha) _fails_ to generates a
% list of exactly one alphabetic character. The default is to generate
% an _alphanumeric_ list, which cannot be a single element.
test(generate_alphanumeric_generate_alpha, [fail]):-
	generate_alphanumeric([generate(alpha)], L)
	,is_set_of(L, 1, alpha).

% Test that specifying only option generate(nums) generates a list of
% exactly two numeric characters
test(generate_alphanumeric_generate_nums, [nondet]):-
	generate_alphanumeric([generate(nums)], L)
	,is_set_of(L, 2, digit).

% Test that specifying only option generate(num) _fails_ to generate a
% list of numeric characters of length exactly one. The default is to
% generate an _alphanumeric_ list which cannot be a single element.
test(generate_alphanumeric_generate_num, [fail]):-
	generate_alphanumeric([generate(num)], L)
	,is_set_of(L, 1, digit).

% Test that specifying only option case(lower) generates a list of
% alphanumeric characters of length exactly 2 where each
% alphabtic characgter is a lowercase letter.
test(generate_alphanumeric_case_lower, [nondet]):-
	generate_alphanumeric([case(lower)], L)
	,is_alnum_set_of_two(L)
	,forall((member(C, L)
		,char_type(C, alpha)
		)
	       ,char_type(C, lower)).

% Same as above but letters are upper case.
test(generate_alphanumeric_case_upper, [nondet]):-
	generate_alphanumeric([case(upper)], L)
	,is_alnum_set_of_two(L)
	,forall((member(C, L)
		,char_type(C, alpha)
		)
	       ,char_type(C, upper)).

% Test that specifying only case(mixed) as an option has the same effect
% as leaving all defaults, really.
test(generate_alphanumeric_case_mixed, [nondet]):-
	generate_alphanumeric([case(mixed)], L)
	,is_alnum_set_of_two(L).

% Test that attempting to generate a 0-length string fails. Because it
% should.
test(generate_alphanumeric_length_0, [fail]):-
	generate_alphanumeric([length(0)], _L).

% Specifying only option length(1) will _fail_ to generate a list of
% exactly one alphabetic character. Because the default is to generate
% an _alphanumeric_ list, a single-element list cannot be generated
% unless one of the options generate[num,alpha]) is explicitly given.
test(generate_alphanumeric_length_1, [fail]):-
	generate_alphanumeric([length(1)], Ls)
	,writeln(Ls)
	,is_set_of(Ls, 1, alpha).

% Test that specifying only option length(2) generates a string of
% length exactly 2.
test(generate_alphanumeric_length_2, []):-
	generate_alphanumeric([length(2)], L)
	,!
	,length(L, 2).

% Test that specifying only option length(3) generates a string of
% length exactly 3. The rest follows by induction.
test(generate_alphanumeric_length_3, []):-
	generate_alphanumeric([length(3)], L)
	,!
	,length(L, 3).

% Specifying only option scrambled(true) generates a string of exactly
% two alphanumeric characters in an undeterminable order (ie, the same
% as for no options).
test(generate_alphanumeric_scrambled_true, [nondet]):-
	generate_alphanumeric([scrambled(true)], L)
	,is_set_of(L, 2, alnum).

% Specifying only option scrambled(false) generates a string of exactly
% two alphanumeric characters ordered by the standard order of terms.
test(generate_alphanumeric_scrambled_false, [nondet]):-
	generate_alphanumeric([scrambled(false)], L)
	,L = [Letter,Number]
	,is_set_of([Letter,Number], 2, alnum)
	,char_type(Letter, alpha)
	,char_type(Number, digit).


/* Test for complete options */

% To generate a list of exactly one lowercase alphabetic character
% specify options length(1) and generate(alpha)
test(generate_alphanumeric_list_of_single_lowercase_alpha, [nondet]):-
	generate_alphanumeric([length(1),generate(alpha)], L)
	,is_set_of(L, 1, alpha)
	,[C] = L
	,char_type(C, lower).

% To generate a list of exactly one uppercase alphabetic character
% specify options length(1), generate(alpha) and case(upper)
test(generate_alphanumeric_list_of_single_uppercase_alpha, [nondet]):-
	generate_alphanumeric([length(1),generate(alpha), case(upper)], L)
	,is_set_of(L, 1, alpha)
	,[C] = L
	,char_type(C, upper).

% To generate a list of exactly 5 lower-case alphabetic characters
% specify options length(5), generate(alphas)
test(generate_alphanumeric_list_of_many_lower_case_alphas, [nondet]):-
	generate_alphanumeric([length(5),generate(alphas)], L)
	,is_set_of(L, 5, alpha)
	,is_set_of(L, 5, lower).

% To generate a list of exactly 5 mixed-case alphabetic characters
% specify options length(5), generate(alphas)
test(generate_alphanumeric_list_of_many_mixed_case_alphas, [nondet]):-
	generate_alphanumeric([length(5),generate(alphas),case(mixed)], Ls)
	,is_set_of(Ls, 5, alpha)
	% This test is a bit dangerous - if it fails, it blocks.
	,findall(C
		,(member(C, Ls)
		 ,char_type(C, lower)
		 )
		,Cs)
	, Cs \== []
	,findall(C
		,(member(C, Ls)
		 ,char_type(C, upper)
		 )
		,Cs_upper)
	,Cs_upper \== [].


% To generate a list of exactly one digit in atomic format specify
% options length(1), generate(num).
test(generate_alphanumeric_list_of_single_atomic_digit, [nondet]):-
	generate_alphanumeric([length(1),generate(num)], L)
	,L = [A]
	,atom(A).

% To generate a list of exactly one digit in numeric format specify
% options length(1), generate(num), number_format(number)
test(generate_alphanumeric_list_of_single_numeric_digit, [nondet]):-
	generate_alphanumeric([length(1),generate(num),number_format(number)], L)
	,L = [D]
	,number(D).




/* Unit test helper predicates */

%!	is_alnum_tuple(+List) is det.
%
%	True when List is an unordered set of exactly one letter
%	and exactly one atomic number.
is_alnum_set_of_two(Ls):-
	sort(Ls, Sorted)
	,Sorted = [Atomic_number,Letter]
	,atom(Letter)
	,atom(Atomic_number)
	,atom_number(Atomic_number, Number)
	,number(Number).


%!	is_alpha_set(+List,+Length,+Character_type) is det.
%
%	True when List is an unordered set of length exactly L and all
%	elements are of the given Character type (as in char_type/2).
is_set_of(Ls, L, Character_type):-
	length(Ls, L)
	,!
	,forall(member(S,Ls),char_type(S, Character_type)).

:-end_tests(generate_alphanumeric).

%!	generate_alphanumeric(-New,+Options) is nondet.
%
%	Generate a list of alphanumeric characters. Options determine
%	the type of characters to produce and whether numbers should be
%	generated as atoms or numbers.
%
%	With no options (Options = []) a list of (not too) randomly
%	selected upper- and lower-case digits, followed by exactly one
%	number is generated with a starting length of 2 increasing on
%	backtracking.
%
%	With option generate(+What) and argument 'alnums' this generates
%	a mix of upper- or lower-case letters and numbers from 0 to 9.
%	With 'alphas' this generates a list of alphanumeric characters.
%	alpha generates a single alphabetic character, generating more
%	on backtracking; nums generates a list of numbers from 0 to 9
%	and num generates a single number from 0 to 9 and more on
%	backtracking.
%
%	Options:
%	* generate(+What), what to generate; one of: [alnums, alphas,
%	alpha, nums, num]. Default is alnums.
%	* case(+Case), the case of alphabetic characters to generate;
%	one of: [upper,lower,mixed]. Default is lower.
%	* number_format(+Format), one of: [atom,number] whether to
%	generate numbers as atoms or numbers. Default is atom.
%	* length(+Length:number), the length of the list to generate;
%	this only makes sense if option generate is given with
%	arguments from the list [alphas,nums,alnums]. The default is to
%	begin generating a single character and increasing the length of
%	the output as backtracking continues.
%       * scramble(+Bool), whether to generate random
%	permutations or alphabetically orderd strings; default is true.
%	Alphabetically ordered strings tend to be long string sof 'a'
%	followed by a single other letter or number.
%
generate_alphanumeric(Options,List):-
	must_be(var, List)
	% Handle length first to avoid blocking on length/2
	,(   selectchk(length(L), Options, Processed_length)
	 ->  (length(Ls, L)
	     ->	  true
	     ;	  !, fail
	     )
	 ;   Ls = [_,_] % default to one letter followed by one number.
	     ,Processed_length = Options
	)
	,(   selectchk(generate(What), Processed_length, Processed_generate)
	->  generate_alphanumeric(What, Ls, Processed_generate)
	;   generate_alphanumeric(alnums, Ls, Processed_length)
	    ,Processed_generate = Processed_length
	)
	,(   \+ selectchk(scramble(false), Processed_generate, _)
	->   random_permutation(Ls, List)
	 ;   List = Ls
	).

generate_alphanumeric(alphas,As,Options):-
	(   memberchk(case(Case), Options)
	->  phrase(letters(Case),As)
	;   phrase(letters(lower), As)
	).

% Hey- this is the same as generate(alphas) + length(1). It's nice to
% have separate clauses to clarify stuff but think about making them
% one.
generate_alphanumeric(alpha,A,Options):-
	(   memberchk(case(Case), Options)
	->  phrase(letter(Case), A)
	;   phrase(letter(lower), A)
	).

generate_alphanumeric(alnums,An,Options):-
	(   memberchk(case(Case), Options)
	->   true
	;   Case = lower
	)
	,(   memberchk(number_format(Format), Options)
	->  true
	;   Format = atom
	)
	,phrase(letters_number(Case, Format), An).

generate_alphanumeric(nums,Ns,Options):-
	(   memberchk(number_format(Format), Options)
	->  phrase(numbers(Format), Ns)
	;   phrase(numbers(atom), Ns)
	).

% See clause for generate(alpha); this is the same as generate(nums) +
% length(1).
generate_alphanumeric(num,N,Options):-
	(   memberchk(number_format(F), Options)
	->  phrase(number(F), N)
	;   phrase(number(atom), N)
	).


%!	letters_number// is nondet.
%
%	A list of one or more lower or upper case letters followed by
%	exactly one number. Use as the name of a nonterminal in the
%	grammar.
letters_number --> letters(_), number(_).

letters_number(Case,Format) --> letters(Case), number(Format).

%!	letters// is nondet.
%
%	One or more upper- or lower- case letters.
letters(Case) --> [L], {phrase(letter(Case), [L])}.
letters(Case) --> letter(Case), letters(Case).

%!	letter// is nondet.
%
%	Exactly one upper- or lower-case letter.
letter(mixed) --> letter(lower) | letter(upper).
letter(lower) --> [a]|[b]|[c]|[d]|[e]|[f]|[g]|[h]|[i]|[j]|[k]|[l]|[m]|[n]|[o]|[p]|[q]|[r]|[s]|[t]|[u]|[v]|[w]|[x]|[y]|[z].
letter(upper) --> ['A']|['B']|['C']|['D']|['E']|['F']|['G']|['H']|['I']|['J']|['K']|['L']|['M']|['N']|['O']|['P']|['Q']|['R']|['S']|['T']|['U']|['V']|['W']|['X']|['Y']|['Z'].

%!	numbers// is nondet.
%
%	One or more single-digit numbers.
numbers(Format) --> [N], {phrase(number(Format), [N])}.
numbers(Format) --> number(Format), numbers(Format).

%!	number// is nondet.
%
%	Exactly one number. Note this actually gets atomic forms of
%	numbers to avoid issues with atom-code conversion.
number(atom) --> ['0']|['1']|['2']|['3']|['4']|['5']|['6']|['7']|['8']|['9'].
number(number) --> [0]|[1]|[2]|[3]|[4]|[5]|[6]|[7]|[8]|[9].


%!	permute(?Xs, ?Ys) is nondet.
%
%	Xs is a permutation of Ys.
%	Suggested here:
%	==
%	http://www.swi-prolog.org/pldoc/doc_for?object=permutation/2
%	==
%
%	Note that mode (-,+) tends to go infinite.
permute([], []).
permute([X|Rest], L) :-
    permute(Rest, L1),
    select(X, L, L1).
