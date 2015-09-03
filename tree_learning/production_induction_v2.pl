:-module(production_induction, [corpus_productions/2]).

:-use_module(configuration).
:-use_module(project_root(utilities)).

/*
findall(C, example_string(C), Cs), corpus_productions(Cs, Ps), !,writeln(Ps).
*/


corpus_productions(Cs, Ps):-
	corpus_grammar(Cs, [_S,_Ns,_Ts,Ps]).



%!	corpus_grammar(+Corpus, ?Grammar) is det.
%
%	Derive a complete Grammar from the set of tokenised examples in
%	Corpus.
%
%	Grammar is a list [S,Ns,Ts,Ps] where S the start symbol in
%	the target language, Ns, the set of Nonterminals, Ts the set
%	of terminals and Ps the set of Productions. Any of the
%	sub-lists of Grammar can be partially instantiated to provide
%	background knowledge to the induction.
%
%	TODO: implement that last bit please :P
%
corpus_grammar(Cs, [S,Ns,Ts_,Ps]):-
	initialisation(Cs, Bs, [S,Ns,Ts,Ps], Ph)
	,derived_terminals(Cs, Ts, Ts_)
	,derived_productions(Cs, Bs, [S,Ns,Ts,Ps], Ph).



%!	initialisation(-Corpus,-Branch_heads,-Grammar,-Branch_head_production) is det.
%
%	Initialises the algorithm's parameters.
%
initialisation(Cs, Bs, G, Ph):-
	findall(C, configuration:example_string(C), Cs)
	,branch_heads(Cs, Bs)
	,grammar(G)
	,start_production(Ph).



/*
findall(C, example_string(C), Cs), production_induction:branch_heads(Cs, Bs).	*/

%!	branch_heads(+Examples, -Branches) is nondet.
%
%	Branches is the set of parse tree branches beginning with the
%	heads of examples in Examples.
branch_heads(Cs, Bs_):-
	branch_heads(Cs, [], Bs)
	% Make into set
	,sort(Bs, Bs_).


%!	branch_heads(+Examples,+Temp,-Acc) is nondet.
%
%	Business end of branch_heads/2.
branch_heads([], Bs, Bs).
branch_heads([[H|_C]|Cs], Bs, Acc):-
	branch_heads(Cs, [H|Bs], Acc).



%!	grammar(-Grammar) is det.
%
%	A grammar for the target language as a tuple
%
%	@TODO: this can use the known-production stuff from
%	learning_productions.
%
grammar([S,Ns,Ts,Ps]):-
	phrase(configuration:start, S)
	,phrase(configuration:nonterminals, Ns)
	,phrase(confirugation:terminals, Ts)
	,Ps = []
	,!. % Lots of unnecessary backtracking here.



%!	derived_terminals(+Corpus,+Given_terminals,-Derived_terminals) is det.
%
%	Extract the set of Terminals in the target grammar from the
%	examples in the Corpus and any Given_terminals.
%
derived_terminals(Cs, Ts, Ts_0):-
	corpus_terminals(Cs, Ts_)
	,ord_union(Ts, Ts_, Ts_0).


%!	corpus_terminals(+Corpus,-Terminals) is det.
%
%	Collect tokens from Corpus to build up the set of Terminals in
%	the target language.
%
corpus_terminals(Corpus, Terminals):-
	flatten(Corpus, Flat)
	,sort(Flat, Terminals).



%!	derived_productions(+Corpus,+Branches,+Grammar,-Branch_head_production) is det.
%
%	Implementation of that_algorithm v 2.2.0
%
%	@TODO: document.
%
derived_productions(Cs, Bs, [_S,_Ns,_Ts,Ps], Ph):-
	node_corpora(Cs, Bs, Cs_)
	,production_composition(Cs_, Bs, Ph, Ps).



/* findall(C, configuration:example_string(C), Cs),production_induction:branch_heads(Cs, Bs), production_induction:node_corpora(Cs, Bs, Cs_).*/

%!	node_corpora(+Corpus,+Branch_heads,-Node_corpora) is det.
%
%	Node_corpora is a list of lists, where each sub-list is a
%	node-corpus that contains all examples that begin with its
%	branch-head and none of the examples that don't.
%
node_corpora(Cs, Bs, Cs_):-
	% NOTE: this is extremely embarassing and inefficient: we go
	% through the whole corpus once for each branch-head. We can do it
	% recursively and save time by removing the examples for each node
	% from the whole corpus. I mean, don't use findall.
	findall(Ci
		,(member(Hi, Bs)
		 ,node_corpus(Hi, Cs, Ci)
		 )
		,Cs_).

/*
findall(C, example_string(C), Cs), production_induction:node_corpus(exile, Cs, Cs_).
	*/

%!	node_corpus(+Branch_head,+Corpus,-Branch_corpus) is semidet.
%
%	Split Corpus to a new Branch_corpus with all the examples that
%	begin with Branch_head and none of the examples that don't.
%
node_corpus(H, Cs, Cs_):-
	node_corpus(H, Cs, [], Cs_).

%!	node_corpus(+Branch_head,+Corpus,+Temp,-Acc) is nondet.
%
%	Business end of node_corpus/3.
node_corpus(_, [], Cs, Cs).
node_corpus(H, [[H|C]|Cs], Cs_, Acc):-
	!, % Avoid backtracking into third clause head.
	node_corpus(H, Cs, [[H|C]|Cs_], Acc).
node_corpus(H, [_C|Cs], Cs_, Acc):-
	node_corpus(H, Cs, Cs_, Acc).


%!	production_composition(+Node_corpora,+Branch_heads,+Branch_head_production,-Node_production) is det.
%
%	Derive productions from the current node corpus and branch
%	heads.
%
production_composition(Cs, Bs, Ph, _Ps):-
	production_composition(Cs,Bs,Ph,[],_Ph_i).

%!	production_composition(+Node_corpora,+Branch_heads,+Branch_head_production,+Temp,-Acc) is nondet.
%
%	Business end of production_composition/4.
%
production_composition([[[C]]],_,Ph,[],Ph_i):-
	augmented_branch_head_production(Ph, [C], Ph_i).
production_composition([[[C|_Cs]]],_,Ph,[],Ph_i):-
	augmented_branch_head_production(Ph, [C], Ph_i).
production_composition([[[Ci|_Cs]|_Exs]|_NCs],[_B],Ph,[],Ph_i):-
	augmented_branch_head_production(Ph, Ci, Ph_i).
production_composition([[[Ci|_Cs]|_Exs]|_NCs],[_B|_Bs],Ph,[],Ph_i):-
	augmented_branch_head_production(Ph, Ci, Ph_i).


beheaded_node_corpus(Cs, Cs_):-
	findall(Cs_r
	       ,member([_|Cs_r], Cs)
	       ,Cs_).

example_heads(Cs, Bs):-
	findall(H
	       ,member([H|_], Cs)
	       ,Bs).


%!	beheaded_example(+Example,-Beheaded) is det.
%
%	Remove the first token of Example.
%
beheaded_example([C], [C]).
beheaded_example([_|C], C).


%!	beheaded_examples(+Example,+Examples,-Together) is det.
%
%	Add Example to the list of beheaded Examples.
%
beheaded_examples(C, Cs, [C|Cs]).


%!	example_head(?Example, ?Head) is det.
%
%	True when Head is the first token in Example. This is just to
%	clarify what's going on in branch_productions/8.
example_head([H|_C], H).


%!	start_production(?Start_production) is det.
%
%	The first branch-head production expanding the start symbol to
%	the empty string.
%
start_production(S --> []):-
	phrase(configuration:start, [S]).


%!	branch_head_production(+Branch_head, -Head_production) is det.
%
%	A new production from the first token in a new branch.
%
branch_head_production(H, H --> [H]).


%!	leaf_production(+Head, -Production) is det.
%
%	A new production from the leaf of a branch
%
leaf_production(H, H --> [H]).


%!	augmented_branch_head_production(+Branch_production,+Leaf_production,-Augmented_branch_production) is det.
%
%	Augment Branch_production with a reference to Leaf_production.
augmented_branch_head_production(Ph --> B , [H], (Ph --> Bs_t)):-
	tree_list(B, Bs)
	,append(Bs, [[H]], Bs_)
	,list_tree(Bs_, Bs_t).
augmented_branch_head_production(Ph --> B , Tp --> _TP_b, (Ph --> Bs_t)):-
	tree_list(B, Bs)
	,append(Bs, [Tp], Bs_)
	,list_tree(Bs_, Bs_t).



/*

%!	derived_productions_(+Node_corpora,+Branches,+Branch_head_production,?Grammar,-Grammar) is det.
%
%	Business end of derived_productions/4
derived_productions([],_, _,[S,Ns,Ts,Ps],[S,Ns,Ts,Ps]).
derived_productions([C|Cs],[H|_Bs],P_h,[S,Ns,Ts,Ps],Acc):-
	branch_head_production(H, P_hi)
	,augmented_branch_head_production(P_h, P_Hi, AP_h)
	,beheaded_node_corpus([C|Cs], Cs_)
	,example_heads([C|Cs],Bs_)
	,derived_productions(Cs_, Bs_, AP_h, [S,Ns,Ts,[P_Hi|Ps]], Acc).

% Kala, koimisou.


	*/



/*


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
branch_productions([[C]], Hp, Cs, Cs, Bs, Bs_, Ps, [Hp,Tp|Ps]):-
	leaf_production(C, Tp)
	,ord_add_element(Bs, C, Bs_).

% A single example left; keep augmenting the branch head production
branch_productions([[_H|C]], Hp, Cs_, Cs_acc, Bs, Bs_acc, Ps, Ps_acc):-
	example_head(C, H_)
	,augmented_branch_production(Hp, [H_], A_Hp)
	,branch_productions([C], A_Hp, Cs_, Cs_acc, Bs, Bs_acc, Ps, Ps_acc).

branch_productions([C|Cs], Hp, Cs_, Cs_acc, Bs, Bs_acc, Ps, Ps_acc):-
	beheaded_example(C, C_)
	,beheaded_examples(C_, Cs_, Cs_beh)
	,example_head(C_, H_)
	,leaf_production(H_, Tp)
	,augmented_branch_production(Hp, Tp, A_Hp)
	,ord_add_element(Bs, H_, Bs_)
	,branch_productions(Cs,Hp,Cs_beh,Cs_acc,Bs_,Bs_acc,[A_Hp|Ps]
			   ,Ps_acc).



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
	node_corpus(H, Cs, Cs_)
	,branch_production(H, Ph)
	,branch_productions(Cs_,Ph,Ps_)
	,append(Ps, Ps_, Ps_0)
	,branches_productions(Cs, Bs, Ps_0, Acc).

branches_productions(Cs, [H|Bs], Ps, Acc):-
	node_corpus(H, Cs, Cs_)
	,branch_production(H, Ph)
	,corpus_productions(Cs_, Ph, [], Cs_beh, [], Bs_from_H, [], Ps_)
	,branches_productions(Cs_beh, Bs_from_H, [], Ps_1)
	,branches_productions(Cs, Bs, [Ps_1|[Ps_|Ps]], Acc).
*/
