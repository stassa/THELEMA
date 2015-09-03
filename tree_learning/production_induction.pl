:-module(production_induction, [corpus_productions/2]).

:-use_module(configuration).
:-use_module(project_root(utilities)).

/*
findall(C, example_string(C), Cs), corpus_productions(Cs, Ps), !,writeln(Ps).
*/


corpus_productions(Cs, Ps):-
	node_heads(Cs, Bs)
	,start_production(Ph)
	,derived_productions(Cs,Bs,Ph,[],Ps).


%!	derived_productions(+Corpus,+Node_heads,+Node_production,+Temp,-Acc) is det.
%
%	Derive Productions from Corpus.
%
%	@TODO: document.
%
derived_productions(_,[],_,Ps,Ps).
derived_productions(Cs,[Hi|Bs],Ph,Ps,Acc):-
	node_corpus(Hi,Cs,Cs_hi)
	,node_production(Cs_hi,[Hi|Bs],Ph,A_Ph,Ph_i)
	,expanded_productions(Ps,Ph_i,A_Ph,Ps_)
	,beheaded_node_corpus(Cs_hi,B_Cs_hi)
	,node_heads(B_Cs_hi,Bs_)
	,derived_productions(B_Cs_hi,Bs_,Ph_i,Ps_,Acc).


%!	node_corpus(+Node_head,+Corpus,-Node_corpus) is semidet.
%
%	Split Corpus to a new Branch_corpus with all the examples that
%	begin with Branch_head and none of the examples that don't.
%
%	@NOTE: if I implement that idea of ordering the corpus by
%	information content/ entropy, I'll have to add a reverse/2 call
%	after the call to node_corpus/4 here, else the minimum entropic
%	ordering will be reversed after it.
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


%!	node_production(+Node_corpus,+Node_heads,+Node_head,+Node_head_production,-Augmented_node_head_production,-Node_production) is nondet.
%
%	@TODO: Document
%
node_production(Cs,Bs,Ph,A_Ph,Ph_i):-
	% Many examples and more than one branch indicate a branch node.
	length(Cs, Cs_l)
	,length(Bs, Bs_l)
	,Cs_l > 1
	,Bs_l > 1
	,[Hi|_] = Bs
	,Ph_i = (Hi --> [Hi])
	,A_Ph = Ph.

node_production(Cs,Bs,Ph,A_Ph,Ph_i):-
	% Many examples and more than one branch indicate a branch node.
	length(Cs, Cs_l)
	,length(Bs, Bs_l)
	,Cs_l > 1
	,Bs_l = 1
	,[Hi|_] = Bs
	,Ph_i = (Hi --> [Hi])
	,A_Ph = Ph.

node_production([[C]],_,Ph,[],Ph_i):-
	augmented_branch_head_production(Ph, [C], Ph_i).

node_production([[C|_Cs]],_,Ph,[],Ph_i):-
	augmented_branch_head_production(Ph, [C], Ph_i).

node_production([[[Ci|_Cs]|_N_Cs]],[_B],Ph,[],Ph_i):-
	augmented_branch_head_production(Ph, Ci, Ph_i).

node_production([[Ci|_Cs]|_N_Cs],[_B|_Bs],Ph,[],Ph_i):-
	augmented_branch_head_production(Ph, Ci, Ph_i).

%!	augmented_branch_head_production(+Branch_production,+Leaf_production,-Augmented_branch_production) is det.
%
%	Augment Branch_production with a reference to Leaf_production.
augmented_branch_head_production(Ph --> [] , T, (Ph --> T)).
augmented_branch_head_production(Ph --> B , [H], (Ph --> Bs_t)):-
	tree_list(B, Bs)
	,append(Bs, [[H]], Bs_)
	,list_tree(Bs_, Bs_t).
augmented_branch_head_production(Ph --> B , Tp, (Ph --> Bs_t)):-
	atomic(Tp) % a terminal
	,tree_list(B, Bs)
	,append(Bs, [Tp], Bs_)
	,list_tree(Bs_, Bs_t).

%!	expanded_productions(+Productions,+Node_production,+Node_head_production,-New_productions) is det.
%
%
expanded_productions(Ps,[],A_Ph,Ps_):-
	ord_add_element(Ps,A_Ph,Ps_).
expanded_productions(Ps,Ph_i,[],Ps_):-
	ord_add_element(Ps, Ph_i,Ps_).
expanded_productions(Ps,Ph_i,A_Ph,Ps_):-
	ord_add_element(Ps, Ph_i,Ps0)
	,ord_add_element(Ps0,A_Ph,Ps_).

%!	beheaded_node_corpus(+Corpus,-Beheaded_corpus) is det.
%
%	@TODO: document
%
beheaded_node_corpus(Cs, Cs_):-
	findall(Cs_r
	       ,member([_|Cs_r], Cs)
	       ,Cs_).

%!	node_heads(+Corpus,-Node_heads) is det.
%
%	@TODO: document.
%
node_heads(Cs, Bs):-
	setof(H
	       ,T^member([H|T], Cs)
	       ,Bs).

%!	start_production(?Start_production) is det.
%
%	The first branch-head production expanding the start symbol to
%	the empty string.
%
start_production(S --> []):-
	phrase(configuration:start, [S]).
