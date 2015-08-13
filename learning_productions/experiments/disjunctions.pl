:-module(disjunctions, [p//0
		       ,augmented_production/3]).

:-use_module('../../utilities.pl').

% Hand crafted version:
%p --> [destroy, target], creature_or_artifact.
%creature_or_artifact --> [creature] | [artifact].

/*
23 ?- phrase(p, P).
P = [destroy, target, creature] ;
P = [destroy, target, artifact].
*/
p-->[destroy, target], creature_or_artifact.
creature_or_artifact-->[creature];[artifact].

/*
21 ?- augmented_production(p --> [destroy,target], creature/artifact, A).
A = (p-->[destroy, target], creature_or_artifact)/ (creature_or_artifact-->[creature];[artifact]).
*/
augmented_production((Name --> Body), Token_1/Token_2
		    ,(Name --> Augmented)/(Disjunction-->[Token_1] ; [Token_2])):-
	tree_list(Body, Tokens)
	,atomic_list_concat([Token_1, or, Token_2], '_', Disjunction)
	,append(Tokens, [Disjunction], Augmented_body)
	,list_tree(Augmented_body, Augmented).


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



