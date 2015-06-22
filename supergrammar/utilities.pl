:-module(utilities, [list_tree/2
		    ,rule_name/1
		    ,generate_alphanumeric/2
		    ,permute/2]).

diff_list(L, Diff):-
	phrase(diff_list(L), Diff, _).

diff_list(L) --> L.

/** <module> Utility predicates used in supergrammar module.
*/

%!	list_tree(+List, -Tree) is nondet.
%
%	Convert a tree to a list. Doesn't go the other way. Don't try
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
