:-module(supergrammar, [generate/4
		       ,generate/3
		       ,rename_rule/3
		       ,add_new_production/3
		       ,given_production/2
		       ,derived_production/2
		       ,bounded_derivation/5
		       ,portray_productions/1
		       ,new_production/4
		       ,clear_productions/0
		       ,remove_rule/1
		       ,erase_rule/1]).

/** <module> Derive production rules from examples and background knowlege.

*This documentation is still under construction - use with caution!*

This module provides predicates for deriving the productions of a
grammar from a list of examples in the target language and
backgroundknowledge about the language consisting of a high-level
specification of the language and a partial grammar for it (given as
terminals and nonterminals in a DCG tree).

The above can be formalised as follows; We are given:

a) A set E of example strings in the target language, for example:

    E : {a, ab, abc, ε}

b) A set Σ of symbols used in strings of the language, for example:

    Σ : {a, b, c, ε}

c) A set G, possibly empty, of lexical types in the target language, for
example:

    G : {g1, g2, g3}

d) A set Γ, possibly empty, of background knowledge about the
target language declared as production rules in a grammar that can be
used to derive strings in the language:

    Γ : { γ -> g | γ ∈ G, g ∈ Σ ∪ G }

e) A specification of the type of the target language given as a
production rule in the same format as the rules in Γ, for example:

    L : { W -> w | W ∈ G, w ∈ Σ ∪ G }

Meaning that W -> w is a production in the grammar that describes the type of
the target language rather than a string in the language.

An example such language-type grammar would be:

    W -> t*nt*.

Meaning the target language can be described by productions where 0 or more
terminal symbols precede exactly one nonterminal symbol followed by 0 or more
terminals.

Given all of the above, we want to generate productions that
a) conform to the specification declared in L,
b) are composed of elements of Γ and G, and
c) can be used to derive each of the examples in E

Som example calls (using the language specified in examples_simple.pl):

Generate all possible rules with complexity 1 (a single terminal or
non terminal in each right-hand side of a rule) and length no more than
3 (no more than 3 tokens parsed):

    forall(generate(1, 3, [bound(upper), bound(upper)]), true).

Generate all possible rules with complexity exactly 3 and length no
more than 3:

    forall(generate(3, 3, [bound(exact), bound(upper)]), true).

Pretty-print a list of productions derived so far.

    portray_productions([print(new)]).

Genearate new productions for testing; they are not saved to the
database at this point:

    new_production(Name,2,Rule,[bound(upper)]).

new_production/4 is nondeterministic; press ';' to see more results at the
listener prompt.

Clear all new productions generated until now:

    clear_tested([clear(productions)]).




*/
:-use_module(utilities).
:-use_module(configuration).

:- dynamic production/3.

%:- meta_predicate supergrammar:portray_production(:).
%:- meta_predicate supergrammar:test(//,//).
%:- meta_predicate supergrammar:add_rule(*,//).
%:- meta_predicate supergrammar:assert_rule(//,:).

% TODO: move these configuration predicates to module configuration -
% they affect induction rather than specific languages.


%!	nonterminal_arity(?Arity) is semidet.
%
%	Rule head arity, including variables automatically added by
%	dcg_translate_rule/2 during grammar clause expansion.
nonterminal_arity(2).


%!	rule_complexity(?Complexity) is semidet.
%
%	Total number of terminals and nonterminals in rule bodies.
rule_complexity(2).


%!	derivation_length(?Length) is semidet.
%
%	Maximum number of tokens in a parsed string. Strings longer than
%	that will fail to parse.
derivation_length(3).


%!	production(?Name,?Clause,?Derivation) is nondet.
%
%	Dynamic predicate used to keep a record of rules already
%	generated, attached to a string derived using the given rule.
%
%	Each production/3 clause is identified by the combination of
%	Name and Clause, in other words a rule must have a unique head
%	and body, but two rules are allowed to parse the same string.
%
%	TODO: decide whether parses must also be unique; find a way to
%	enforce this if so.
%
production(nil, nil, []).


%!	derived_grammar(?Grammar) is det.
%
%	Name of the top-level production of the grammar we are trying
%	to learn, used to connect new rules to each other and the
%	partial grammar in example worlds.
%
%	Setting this to 'nonterminal' will allow new productions to be
%	added to further derived productions.
%
%	TODO: The above might also introduce cyclic rules of the form,
%	eg:
%	a0 --> a0, a1.
%
%	I'm not actually sure it will- find out and if yes, stop it.
%
derived_grammar(nonterminal).


%!	generate(+Rule_complexity,+Derivation_length,+Inference_limit,+Options) is det.
%
%	As generate/3 but with Inference_limit; also prints out time
%	statistics (output of time/1).
generate(Rule_complexity, Derivation_length, Inference_limit, Options):-
	time(call_with_inference_limit(
	    (   generate(Rule_complexity, Derivation_length, Options)
	       ,fail
	    )
	    ,Inference_limit
	    ,Result))
	,writeln(result:Result)
	,portray_productions([print(new)])
	,!.
generate(_, _, _, _):-
	writeln(result:exited_before_reaching_inference_limit)
	% If (generate/3, fail) above exits before the given inference limit
	% then generate/4 will fail also. In that case, we want to fall here
	% to allow the found productions to be portrayed.
	,portray_productions([print(new)]).

%!	generate(+Rule_complexity,+Derivation_length,+Options) is det.
%
%	Start a generate-and-test cycle with the given hyperparameters.
%
%	Rule_complexity: the total number of terminals and nonterminals
%	allowed in the right-hand side of a production.
%
%	Derivation_length: the length of tokens allowed in a string.
%
%	Options control the type of bound for the two hyperparameters.
%	'upper' means "X or less"; 'exact' means duh.
%
%	Current options are:
%	* bound(+Derivation:number), passed to bounded_derivation/5
%	* bound(+Rule_length:number), passed to bounded_production/4
%
generate(C, Dl, Options):-
	(   selectchk(derivation_bound(D_bound), Options, Options1)
	->  true
	;   D_bound = upper % default is to set upper bounds
	    ,Options1 = Options
	)
	,(   selectchk(production_bound(P_bound), Options1, Options2)
	 ->  true
	 ;   P_bound = upper
	    ,Options2 = Options1
	)
	,(   memberchk(ground(Ground), Options2)
	 ->  true
	 ;   Ground = false % default is to not ground rules
	 )
	,clear_productions
	,new_production(N, C, R, [production_bound(P_bound)])
	,bounded_derivation(R, Dl, D, _Rest, [derivation_bound(D_bound),ground(Ground)])
	,example_string(Str)
	,test(N, D, Str)
	,rename_rule(R, Nn, Rr)
	,add_new_production(Nn, Rr, D)
	,configuration:examples_module(M)
	,listing(M:Nn).


%!	test(+Rule_name,+Derivation,+Example) is det.
%
%	Test a new rule for addition to the database. If Derivation
%	matches Example the new rule is fit for addition to the
%	database. Otherwise, we 'll get a new one.
%
%	test/3 outputs a message to the user to indicate whether the
%	given rule parsed an example or not and then goes back for more
%	(more new rules that is).
%
test(R, P, P):-
	writeln('Successfully parsed ':P-with:R-->P).
test(R, P, Str):-
	writeln('Failed to parse ':Str-with:(R -->P))
	,fail.


%!	rename_rule(+Rule,-New_name,-New_rule) is det.
%
%	Rename a rule, giving it a new head with the same arguments and
%	a new functor. Don't ask.
%
rename_rule((H:-T), N, (H1:-T)):-
	H =.. [_|Args]
	,rule_name(N)
	,\+ production(N,_,_)
	,!
	,\+ production(_, (_:-T), _)
	,H1 =.. [N|Args].


%!	add_new_production(+Name,+Body,+Derivation) is det.
%
%	Add a newly-derived production to the database and connect it to
%	the partial grammar as the right-hand side of a nonterminal//0
%	clause, unless such a clause already exists.
%
add_new_production(N, R, D):-
	% Connect the rule to the grammar derived so far.
	% Unless it's already connected (not sure that this will ever be the case)
	derived_grammar(G)
	% Wrapp in []'s to represent double-barring I guess.
	,dcg_translate_rule(G --> [N], Rr)
	,(   \+ derived_production(N, _)
	 ->  configuration:examples_module(M)
	 % Assert the new nonterminal//0 clause in the examples module.
	    ,assert(M:Rr)
	 ;   true
	 )
	% Add the new production to the examples module- not in this module!
	,assert(M:R)
	% Add the new production to the set of known productions, in this module.
	,assert(production(N, R, D))
	,! % Green cut
	.



%!	portray_productions(+Options) is det.
%
%	Pretty print known and/or newly found productions in the target
%	language.
%
%	Options:
%	  print(+What), what to print. One of: [given, new all].
%
portray_productions(Options):-
	(   (memberchk(print(given), Options)
	    ;	memberchk(print(all), Options)
	    )
	->	setof(P,D^given_production(P,D),Given)
	       ,forall(member(P,Given),portray_production(P))
	;   true
	)
	,(   (memberchk(print(new), Options)
	     ;	 memberchk(print(all), Options)
	     )
	 ->	configuration:examples_module(M)
	       ,setof(N,D^derived_production(N, D),Ns)
	       ,forall(member(N, Ns),portray_production(M:N))

	 ;   true
	 ).



%!	given_production(?Production,?Derivation) is nondet.
%
%	True when Production is a known grammar rule that is part of
%	the grammar of the target language and Derivation is a
%	derivation of that rule.
%
%	In other words, Production --> Derivation is part of our
%	background knowledge about the target language.
%
given_production(P, D):-
	(   phrase(nonterminal, [P])
	,\+ production(P, _, _)
	)
	,rule_complexity(C)
	,length(Args, C)
	,H =.. [P|Args]
	,clause(H, _)
	,phrase(P, D).


%!	derived_production(?Production,?Derivation) is nondet.
%
%	True when Production_name is the name of a newly-derived grammar
%	rule and Derivation its derivation. All Derivations of
%	Production are generated on subsequent backtracking.
%
%	Note that all new productions are connected to the target
%	language via nonterminal//0, in other words:
%
%	nonterminal --> Production.
%
%	- is true.
%
%	TODO: See notes inline.
%
derived_production(P, D):-
	(   phrase(nonterminal, [P])
	,   production(P, _, _)
	)
	% Hey, do I need to do all this stuff? Can't I just get the rule Head from production/3?
	,rule_complexity(C)
	,length(Args, C)
	,H =.. [P|Args]
	,configuration:examples_module(M)
	,clause(M:H, _)
	% phrase/2 leaves out stochastic_supergrammar rules
	% Where the "Rest" clause is the production Score.
	% Think of whether this should be allowed or not.
%	,phrase(M:P, D).
	,phrase(M:P, D, _).


%!	portray_production(+Name) is det.
%
%	Business end of portray_productions/1. Just a wrapper around
%	listing/1 to stop it blowing up when encountering production (or
%	derivation) names that are not known predicates, as is the case
%	with the "seed" derivation:
%	==
%	  derivation(nil, []).
%	==
%
%	-which is added to the database to initiate backtracking in
%	generate/3.
%
portray_production(supergrammar:nil):-!.
portray_production(Name):-
	listing(Name).


%!	bounded_derivation(+Rule,+Bound,-Derivation,-Rest,+Options) is det.
%
%	Generate a Derivation using Rule, binding the length of the
%	token string to Bound.
%
%	Rule is a predicate in the form: (Head:-Body). Rest is a list of
%	tokens allowed to follow as input after Rule has successfully
%	parsed Derivation- think of the third argument in phrase/3.
%
%	Current Options:
%	* bound(B); whether Bound sets an upper limit to the length of
%	Derivation, or an absolute length requirement. One of: [upper,
%	exact] (for an upper and absolute limit respectively).
%
bounded_derivation(R, L, D, Rest, [derivation_bound(upper),Ground]):-
	length(D, L1)
	,(L1 =< L
	 ->  true
	 ;   !, fail
	 )
	,derivation(R, D, Rest, [Ground]).

bounded_derivation((H:-T), L, D, Rest, [derivation_bound(exact),Ground]):-
	once(length(D, L))
	,derivation((H:-T), D, Rest, [Ground]).


%!	derivation(+Rule,-Derivation,-Rest, Options) is nondet.
%
%	Business end of bounded_derivation/5.
%
%	Derivation is a difference list; remember that you can get rid
%	of the variable in the tail by binding 'Rest' to [[]]:
%
%	Options:
%	  ground(+Boolean): whether to bind variables in heads and
%	  bodies in ground terms or not.
%
derivation((H:-T), D, Rest, [ground(false)]):-
	duplicate_term((H:-T), (Hh:-Tt))
	,configuration:examples_module(M)
	,M:Tt % Call the tail
	,Hh =.. [_N|[D|Rest]]. % Head is now bound

% TODO: make this an option (ie, no vars in rule bodies)
derivation((H:-T), D, Rest, [ground(true)]):-
	configuration:examples_module(M)
	,M:T % Call the tail
	,H =.. [_N|[D|Rest]]. % Head is now bound


%!	new_production(?Name,+Complexity,-Rule,+Options) is nondet.
%
%	Create a new production with the given Name as its head.
%	If Name is unbound, a new Name will be generated using
%	rule_name/1.
%
%	Complexity is the total number of terminals and nonterminals
%	allowed in the right-hand side of the new rule.
%
%	Rule is bound to a new rule; for example:
%
%	==
%	?- new_production(a0, 2, R, _).
%       R = (a0(_G1336, _G1337):-g1(_G1336, _G1337)) .
%       ==
%
%       Current options:
%	* bound(B); whether Complexity is an upper bound or an absolute
%	length. One of: upper, exact.
%
%      KLUDGE: N is output, not a parameter- move after L (first
%      actual parameter).
%
%
new_production(N, L, R, Options):-
	once(rule_name(N))
	,bounded_production(N, L, R, Options).


%!	bounded_production(+Name,+Bound,-Rule,Options) is nondet.
%
%	Business end of new_production/4. Same thing but one clause per
%	option.
%
bounded_production(N, L, R, [production_bound(upper)]):-
	length(P, L1)
	,(   L1 =< L
	 ->  true
	 ;   !, fail
	 )
	,phrase(language, P)
	,list_tree(P, T)
	,dcg_translate_rule(N --> T, R).

bounded_production(N, L, R, [production_bound(exact)]):-
	length(P, L)
	,phrase(language, P)
	,list_tree(P, T)
	,dcg_translate_rule(N --> T, R).


%!	clear_productions is det.
%
%	Clear the database from productions generated in the last
%	generate-and-test cycle.
%
%	Because of the liberal use of dynamic predicates to keep track
%	of productions already derived (people tried to warn you but you
%	just wouldn't listen would you?) clearing the database must
%	happen in a very specific order. This is particularly the case
%	because of the reliance of derived_production/2 on production/3
%	to find newly derived productions.
%
%	The correct order to clear dynamic clauses is as follows:
%
%	1. First clear clauses of nonterminal//0 connecting newly
%	   derived productions to the grammar, e.g. clauses like:
%
%	   ==
%	   nonterminal --> [a0].
%	   ==
%
%	2. Next clear the actual clauses of the newly derived
%	   productions, connected to the grammar via nonterminal//0
%	   references as above, e.g. clauses like:
%	   ==
%	   a0(A,B):-
%	     g1(A,B).
%	   ==
%
%	3. Finally, clear clauses of production/3 that reference a newly
%	   derived production, e.g:
%	   ==
%	   production(a0, a0(A,B):-g1(A,B), [a]).
%	   ==
%
clear_productions:-
	clear_derived_productions(references)
	,clear_derived_productions(clauses)
	,clear_derived_productions(production_3).


%!	clear_derived_productions(+What) is det.
%
%	Business end of clear_productions/0. What determines what to
%	clear and is one of:
%	* references; clear all clauses of nonterminal//0 that reference
%	a newly derived production.
%	* clauses; clear all clauses of newly derived productions
%	referenced by nonterminal//0
%	* production_3; clear all clauses of production/3 except for the
%	"seed" clause production(nil, [], []).
%
clear_derived_productions(references):-
	derived_grammar(G)
	,configuration:examples_module(M)
	,forall(derived_production(N, _),
		(   dcg_translate_rule(G --> [N], H:-B)
		   ,(   clause(M:H, B, Ref)
		   ->	erase(Ref)
		    ;	true
		    )
		)
	       ).

clear_derived_productions(clauses):-
	rule_complexity(C)
	,configuration:examples_module(M)
	,forall(production(N, _, _),
		(  functor(Nt,N,C)
		   ,(   clause(M:Nt, _, Ref)
		   ->	erase(Ref)
		    ;	true
		    )
		)
	       ).

clear_derived_productions(production_3):-
	forall(production(N, _, _)
	       ,(
		   (   N \== nil
		   ->  retract(production(N,_,_))
		   ;   true
		   )
		)
	      ).



%!	remove_rule(+Rule_name) is det.
%
%	Retract all clauses with the given name and arity as in
%	nonterminal_arity(A) from the database.
%
remove_rule(R):-
	nonterminal_arity(A)
	,functor(T,R,A)
	,retractall(T).

%!	erase_rule(+Rule_name) is det.
%
%	As remove_rule/1 but abolishes clauses rather than retracting.
erase_rule(R):-
	nonterminal_arity(A)
	,abolish(R/A).



