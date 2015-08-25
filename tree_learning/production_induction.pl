:-module(production_induction, [corpus_productions/2]).

:-use_module(configuration).


/*
findall(C, example_string(C), Cs), corpus_productions(Cs, Ps), !,writeln(Ps).
*/

corpus_productions(Cs, Ps):-
	corpus_branches(Cs, Bs)
	,branches_productions(Cs, Bs, Ps).


/*
findall(C, example_string(C), Cs), production_induction:corpus_branches(Cs, Bs).	*/

%!	corpus_branches(+Examples, -Branches) is nondet.
%
%	Branches is the set of parse tree branches beginning with the
%	heads of examples in Examples.
corpus_branches(Cs, Bs_):-
	corpus_branches(Cs, [], Bs)
	% Make into set
	,sort(Bs, Bs_).

%!	corpus_branches(+Examples,+Temp,-Acc) is nondet.
%
%	Business end of corpus_branches/2.
corpus_branches([], Sb, Bs):-
	reverse(Sb, Bs).
corpus_branches([[H|_C]|Cs], Bs, Acc):-
	corpus_branches(Cs, [H|Bs], Acc).


%!	branches_productions(+Examples, +Branches, -Productions) is nondet.
%
%	Infer a tree of Productions from Branches in Examples.
%	Clearly this needs better documentation :P
branches_productions(Cs, Bs, Ps):-
	branches_productions(Cs, Bs, [], Ps).


%!	branches_productions(+Corpus,+Branches,+Temp,-Acc) is nondet.
%
%	Business end of branches_productions/3.
%
branches_productions(_, [], Ps, Ps_):-
	% sort to remove accumulated duplicates
	sort(Ps, Ps_).
branches_productions(Cs, [H|Bs], Ps, Acc):-
	branch_corpus(H, Cs, Cs_)
	,branch_production(H, Ph)
	,branch_productions(Cs_,Ph,Ps_)
	,append(Ps, Ps_, Ps_0)
	,branches_productions(Cs, Bs, Ps_0, Acc).

/*
branches_productions(Cs, [H|Bs], Ps, Acc):-
	branch_corpus(H, Cs, Cs_)
	,branch_production(H, Ph)
	,corpus_productions(Cs_, Ph, [], Cs_beh, [], Bs_from_H, [], Ps_)
	,branches_productions(Cs_beh, Bs_from_H, [], Ps_1)
	,branches_productions(Cs, Bs, [Ps_1|[Ps_|Ps]], Acc).
*/

/*
findall(C, example_string(C), Cs), production_induction:branch_corpus(exile, Cs, Cs_).
	*/

%!	branch_corpus(+Branch_head,+Corpus,-Branch_corpus) is semidet.
%
%	Split Corpus to a new Branch_corpus with all the examples that
%	begin with Branch_head and none of the examples that don't.
%
branch_corpus(H, Cs, Cs_):-
	branch_corpus(H, Cs, [], Cs_).

%!	branch_corpus(+Branch_head,+Corpus,+Temp,-Acc) is nondet.
%
%	Business end of branch_corpus/3.
branch_corpus(_, [], Cs, Cs).
branch_corpus(H, [[H|C]|Cs], Cs_, Acc):-
	!, % Avoid backtracking into third clause head.
	branch_corpus(H, Cs, [[H|C]|Cs_], Acc).
branch_corpus(H, [_C|Cs], Cs_, Acc):-
	branch_corpus(H, Cs, Cs_, Acc).


%!	examples_branches(+Corpus,+Head_production,-Productions) is nondet.
%
%	Construct productions for the current branch.
branch_productions(Cs,Ph,Ps):-
	branch_productions(Cs, Ph, [], Cs_, [], Bs, [], Ps_0)
	,branches_productions(Cs_, Bs, Ps_1)
	,append(Ps_0, Ps_1, Ps).


%!	branch_productions(+Corpus,+Head_production,+Temp,-Acc) is nondet.
%
%	Business end of branch_productions/3.
branch_productions([], _, Cs, Cs, Bs, Bs, Ps, Ps).

% Just one token left- only add a leaf production.
branch_productions([[C]], _Hp, Cs, Cs, Bs, Bs_, Ps, [Tp|Ps]):-
	leaf_production(C, Tp)
	,ord_add_element(Bs, C, Bs_).

branch_productions([C|Cs], Hp, Cs_, Cs_acc, Bs, Bs_acc, Ps, Ps_acc):-
	beheaded_example(C, C_)
	,beheaded_examples(C_, Cs_, Cs_beh)
	,example_head(C_, H_)
	,leaf_production(H_, Tp)
	,augmented_branch_production(Hp, Tp, A_Hp)
	,ord_add_element(Bs, H_, Bs_)
	,branch_productions(Cs,Hp,Cs_beh,Cs_acc,Bs_,Bs_acc,[A_Hp|Ps]
			   ,Ps_acc).


beheaded_example([_|C], C).

beheaded_examples(C, Cs, [C|Cs]).

example_head([H|_C], H).

%!	branch_production(+Branch_head, -Head_production) is det.
%
%	A new production from the first token in a new branch.
branch_production(H, H --> [H]).

%!	leaf_production(+Head, -Production) is det.
%
%	A new production from the leaf of a branch
leaf_production(H, H --> [H]).

%!	augmented_branch_production(+Branch_production,+Leaf_production,-Augmented_branch_production) is det.
%
%	Augment Branch_production with a reference to Leaf_production.
augmented_branch_production(Ph --> [Ph] , Tp --> [_TP], (Ph --> [Ph], Tp)).

/*

  0) Initialise:
     C, a set of tokenised examples in the target language
     B, the set of heads of branches (a.k.a. names of nodes) from the
     current node in the induced parse tree
     G, the target grammar, a quadruple: [S,N,T,P] where S the start
     symbol, N the set of nonterminals, T the set of Terminals and P the
     set of Productions.

  1) For each example c in C:
     2) Take h, the head of c
     3) Add h to B
	> At the beginning of a run, the first node is s, the start symbol
	for G.
  4) For each node h in B
     5) Split C to C',the corpus for h: a new corpus with all the
     examples in C that begin with h and none of the examples that
     don't.

     6) Create ph, the head-production for h: a production named as h
        expanding to a literal h as a terminal:
	ph = h --> [h].

     7) For each example c' in C':
        8) Behead c': remove its head, h
	9) If c' = [] end loop.
	    * Or, we could end the loop when length(c') = 1; see notes.
	10) Add the beheaded c', c" to C", the set of beheaded examples in C'

        11) Take h', the new head of c"
	12) Create pt the tail-production for h': a production named as h'
	    expanding to a literal h' as a terminal:
	    pt = h' --> [h'].

	13) Look-back/retraugment: create a new clause for head-production
	ph starting with the head-production ph augmented with a reference
	to the tail-production, pt:
	    ph' = h --> [h], pt.

	14) Add h' to B' the set of heads of branches from h
     15) Repeat from 1) with B = B' and C = C"

*/


