:-module(disjunctions, [p//0
		       ,augmented_production/3]).

:-use_module('../../utilities.pl').

% Hand crafted version, single disjunction:
%p --> [destroy, target], creature_or_artifact.
%creature_or_artifact --> [creature] | [artifact].

% Handcrafted version, continuing with disjunction.
p0-->[destroy, all], artifacts, (e0;and,enchantments).
artifacts --> [artifacts].
and --> [and].
e0 --> [].
enchantments --> [enchantments].


/*  15/08 */


/*

This looks right:

[debug] 64 ?- notrace, augmented_production((p-->[t_destroy, all_artifacts], ([epsilon];[t_and])), [t_enchantments], A).
A = (p-->[t_destroy, all_artifacts], ([epsilon];[t_and], [t_enchantments])).

p-->[t_destroy, all_artifacts], ([epsilon];[t_and], [t_enchantments]).

[debug] 68 ?- phrase(p, P).
P = [t_destroy, all_artifacts] ;
P = [t_destroy, all_artifacts, t_and, t_enchantments].


Also with [] instead of epsilon//0 if needed:

[debug] 76 ?- notrace, augmented_production((p-->[t_destroy, all_artifacts]), []/t_and, A).
A = (p-->[t_destroy, all_artifacts], ([];t_and)).


[debug] 77 ?- notrace, augmented_production((p-->[t_destroy, all_artifacts], ([];t_and)), [t_enchantments], A).
A = (p-->[t_destroy, all_artifacts], ([];t_and, [t_enchantments])).

p-->[t_destroy, all_artifacts], ([];[t_and], [t_enchantments]).

[debug] 79 ?- phrase(p, P).
P = [t_destroy, all_artifacts] ;
P = [t_destroy, all_artifacts, t_and, t_enchantments].
*/

p-->[t_destroy, all_artifacts], ([epsilon];[t_and], [t_enchantments]).


augmented_production(Name --> (B_0, (B_1;B_2))
		    ,Token, Name --> (B_0, (B_1;B_2_augmented)) ):-
	!,
	tree_list(B_2, Tokens)
	,append(Tokens, [Token], B_2_Token)
	,list_tree(B_2_Token, B_2_augmented).
augmented_production(Name --> Body, Token_1/Token_2, Name --> Augmented):-
	!,
	tree_list(Body, Tokens)
	,append(Tokens, [Token_1 ; Token_2], Augmented_body)
	,list_tree(Augmented_body, Augmented).
augmented_production(Name --> Body, Token, Name --> Augmented):-
	tree_list(Body, Tokens)
	,append(Tokens, [Token], Augmented_body)
	,list_tree(Augmented_body, Augmented).


/*
p0//0 above is right but this is what I get from
augmented_production/3 so far:

[debug] 52 ?- notrace, augmented_production((p-->[t_destroy, all_artifacts], (epsilon;t_and)), t_enchantments, A).
A = (p-->[t_destroy, all_artifacts], (epsilon;t_and), t_enchantments).

Which is wrong.

Also note it blows the nonterminals-then-terminals thing out of the
water.

Also also, I got my compression levels wrong- if t_destroy and
all_artifacts are terminals then epsilon, t_and and t_enchantments
should also. If they're nonterminals, same.
*/

/*  14/08 - see notes etc.*/

/*
18 ?- augmented_production((p-->[destroy,all]),t_artifacts/t_and,P ).
P = (p-->[destroy, all], t_artifacts_or_t_and)/ (t_artifacts_or_t_and-->t_artifacts;t_and).

p-->[destroy, all], t_artifacts_or_t_and.
t_artifacts_or_t_and-->t_artifacts;t_and.
% This is assuming there already are pre-terminals for t_artifacts and
% t_and in the database.
t_artifacts --> [artifacts].
t_and --> [and].

But, the result is no good:
20 ?- phrase(p, P).
P = [destroy, all, artifacts] ;
P = [destroy, all, and].

So this is what we should be calling:
22 ?- augmented_production((p-->[destroy,all],t_artifacts),epsilon/t_and,P ).
P = (p-->[destroy, all], t_artifacts, epsilon_or_t_and)/ (epsilon_or_t_and-->epsilon;t_and).
*/
%p-->[destroy, all], t_artifacts, epsilon_or_t_and.
%epsilon_or_t_and-->epsilon;t_and.
% Assuming these are declared etc.
%t_artifacts --> [artifacts].
%t_and --> [and].
%epsilon --> [].

/*
And that:

24 ?- phrase(p, P).
P = [destroy, all, artifacts] ;
P = [destroy, all, artifacts, and].

- is alright because we want to keep augmenting both branches. Though
the first branch should fail to augment?
*/

/* This is OK- ish. It needs an extra clause (to deal with conjunctions)
but it will generate two rules, which makes it harder to score. I
think I should favour the version that generates a single rule with
the whole of the disjunction. The bad thing is that it won't work if you
want to have more than a single disjunction. See latest, 15/08.

augmented_production_((Name --> Body), Token_1/Token_2
		    ,(Name --> Augmented)/(Disjunction--> Token_1 ; Token_2)):-
	tree_list(Body, Tokens)
	,(is_list(Token_1) -> [T1] = Token_1 ; T1 = Token_1)
	,(is_list(Token_2) -> [T2] = Token_2 ; T2 = Token_2)
	,atomic_list_concat([T1, or, T2], '_', Disjunction)
	,append(Tokens, [Disjunction], Augmented_body)
	,list_tree(Augmented_body, Augmented).

*/
/*

Same as above, but one-clause version:

26 ?- augmented_production((p-->[destroy,all],t_artifacts),epsilon/t_and,P ).
P = (p-->[destroy, all], t_artifacts, (epsilon;t_and)).


p-->[destroy, all], t_artifacts, (epsilon;t_and).
t_artifacts --> [artifacts].
t_and --> [and].
epsilon --> [].

And again:
28 ?- phrase(p, P).
P = [destroy, all, artifacts] ;
P = [destroy, all, artifacts, and].


augmented_production(Name --> Body, Token_1/Token_2, Name --> Augmented):-
	tree_list(Body, Tokens)
	,append(Tokens, [Token_1 ; Token_2], Augmented_body)
	,list_tree(Augmented_body, Augmented).
*/


/* 13/08 - see notes of that day.*/

/*
23 ?- phrase(p, P).
P = [destroy, target, creature] ;
P = [destroy, target, artifact].

p-->[destroy, target], creature_or_artifact.
creature_or_artifact-->[creature];[artifact].


Oh, err: this won't work with a token that's a nonterminal. But you can
fix it. I know you can.

21 ?- augmented_production(p --> [destroy,target], creature/artifact, A).
A = (p-->[destroy, target], creature_or_artifact)/ (creature_or_artifact-->[creature];[artifact]).

augmented_production((Name --> Body), Token_1/Token_2
		    ,(Name --> Augmented)/(Disjunction-->[Token_1] ; [Token_2])):-
	tree_list(Body, Tokens)
	,atomic_list_concat([Token_1, or, Token_2], '_', Disjunction)
	,append(Tokens, [Disjunction], Augmented_body)
	,list_tree(Augmented_body, Augmented).
*/

/*
Works just like the hand-crafted version:

25 ?- augmented_production(p --> [destroy,target], creature/artifact, A).
A = (p-->[destroy, target], ([creature];[artifact])).

11 ?- phrase(p, P).
P = [destroy, target, creature] ;
P = [destroy, target, artifact].

p-->[destroy, target], ([creature];[artifact]).

augmented_production((Name --> Body), Token_1/Token_2, (Name --> Augmented)):-
	tree_list(Body, Tokens)
	,append(Tokens, [[Token_1] ; [Token_2]], Augmented_body)
	,list_tree(Augmented_body, Augmented).
*/



