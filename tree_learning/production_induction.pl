:-module(production_induction, [corpus_productions/2]).

:-use_module(configuration).
:-use_module(project_root(utilities)).

/*
findall(C, example_string(C), Cs), corpus_productions(Cs, Ps), !,writeln(Ps).
*/


corpus_productions(Cs, Ps_):-
	node_heads(Cs, Bs)
	,start_production(Ph)
	,derived_productions(Cs,Bs,Ph,[],Ps)
	,sort(Ps, Ps_).


%!	derived_productions(+Corpus,+Node_heads,+Node_production,+Temp,-Acc) is det.
%
%	Derive Productions from Corpus.
%
%	@TODO: document.
%	@TODO: don't use append at first clause- is there fore clarity
%	only. But try to preserve the order of rules derivation because
%	it's nice to read them like that.
%
/*derived_productions(_,[],Ph,Ps,Ps_):-
	append(Ps, [Ph], Ps_).
derived_productions(Cs,[Hi|Bs],Ph,Ps,Acc):-
	node_corpus(Hi,Cs,Cs_hi)
	,node_production(Cs_hi,[Hi|Bs],Ps,Ph,Ps_,Ph_i)
	,beheaded_node_corpus(Cs_hi,B_Cs_hi)
	,node_heads(B_Cs_hi,Bs_hi)
	% Follow current branch
	,derived_productions(B_Cs_hi,Bs_hi,Ph_i,Ps_,Ps_hi)
	% Follow subsequent branches
	,derived_productions(Cs,Bs,Ph,Ps_hi,Acc).
*/
%derived_productions(_,_,[],Ps,Ps).
%derived_productions([],_,_,Ps,Ps).
%derived_productions(_,[],_,Ps,Ps).

derived_productions([],_,_,Ps,Ps).

/*
derived_productions([[_C]],[Hi],Ph,Ps,[A_Ph|Ps]):-
	% A leaf node
	augmented_node_head_production(Ph, [Hi], A_Ph).

derived_productions([[_|[H|Cs]]],[Hi],Ph,Ps,Acc):-
	% A stem node (single example, single branch)
	augmented_node_head_production(Ph, [Hi], A_Ph)
	,derived_productions(Cs, [H], A_Ph, Ps, Acc).

derived_productions([C|Cs],[Hi],Ph,Ps,Acc):-
	% A branch node (multiple examples for a single branch)
	Ph_i = (Hi --> [Hi])
	,augmented_node_head_production(Ph, Hi, A_Ph)
	,beheaded_node_corpus([C|Cs],B_Cs)
	,node_heads(B_Cs, Bs_Hs)
	,derived_productions(B_Cs, Bs_Hs, Ph_i, [A_Ph|Ps], Acc).
*/
derived_productions(Cs,[Hi|Bs],Ph,Ps,Acc):-
	you_are_here(4),
	Ph_i = (Hi --> [Hi])
	,augmented_node_head_production(Ph, Hi, A_Ph)
	,split_corpus(Hi,Cs,Cs_hi,Cs_Rest)
	,beheaded_node_corpus(Cs_hi,B_Cs_hi)
	,node_heads(B_Cs_hi,Bs_hi)
	,derived_productions(B_Cs_hi,Bs_hi,Ph_i,[A_Ph|Ps],Ps_hi)
	,derived_productions(Cs_Rest,Bs,Ph,Ps_hi,Acc).

you_are_here(_).

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


%!	split_corpus(+Node_head,+Corpus,-Node_corpus,-Rest_of_corpus) is semidet.
%
%	Same as node_corpus/3 but also binds the Rest_of_corpus to the
%	list of examples that don't begin with Node_head.
%
split_corpus(H, Cs, Cs_H, Cs_Rest):-
	split_corpus(H, Cs, [], Cs_H, [], Cs_Rest).

%!	split_corpus(+Branch_head,+Corpus,+Temp,-Acc) is nondet.
%
%	Business end of split_corpus/3.
split_corpus(_, [], Cs_H, Cs_H, Cs_Rest, Cs_Rest).
split_corpus(H, [[H|C]|Cs], Cs_H, Cs_H_Acc, Cs_Rest, Cs_Rest_Acc):-
	!, % Avoid backtracking into third clause head.
	split_corpus(H, Cs, [[H|C]|Cs_H], Cs_H_Acc, Cs_Rest, Cs_Rest_Acc).

split_corpus(H, [C|Cs], Cs_H, Cs_H_Acc, Cs_Rest, Cs_Rest_Acc):-
	split_corpus(H, Cs, Cs_H, Cs_H_Acc, [C|Cs_Rest], Cs_Rest_Acc).


%!	augmented_node_head_production(+Branch_production,+Leaf_production,-Augmented_branch_production) is det.
%
%	Augment Branch_production with a reference to Leaf_production.
augmented_node_head_production(Ph --> [] , H, (Ph --> H)).
augmented_node_head_production(Ph --> [T] , [H], (Ph --> [T,H])).
augmented_node_head_production(Ph --> [T] , H, (Ph --> [T],H)):-
	% not a terminal; not checking with atomic/1
	% because of tokens like 'and/or' etc that are not (atomic).
	\+ is_list(H).
augmented_node_head_production(Ph --> B , [H], (Ph --> Bs_t)):-
	tree_list(B, Bs)
	,append(Bs, [[H]], Bs_)
	,list_tree(Bs_, Bs_t).
augmented_node_head_production(Ph --> B , H, (Ph --> Bs_t)):-
	% not a terminal
	\+ is_list(H)
	,tree_list(B, Bs)
	,append(Bs, [H], Bs_)
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
beheaded_node_corpus([[_]], []).
beheaded_node_corpus(Cs, Cs_):-
	findall(Cs_r
	       ,(member([_|Cs_r], Cs)
		,Cs_r \= []
		)
	       ,Cs_).


%!	node_heads(+Corpus,-Node_heads) is det.
%
%	@TODO: document.
%
node_heads([], []).
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


/*
derived_productions(Cs,[Hi|Bs],Ph,Ps,Acc):-
	% A branch node
	Ph_i = (Hi --> [Hi])
	,augmented_node_head_production(Ph, Hi, A_Ph)
	,split_corpus(Hi,Cs,Cs_hi,Cs_Rest)
	,beheaded_node_corpus(Cs_hi,B_Cs_hi)
	,node_heads(B_Cs_hi,Bs_hi)
	,derived_productions(B_Cs_hi,Bs_hi,Ph_i,[A_Ph|Ps],Ps_hi)
	% would be nice to drop the examples processed already from Cs
	,derived_productions(Cs_Rest,Bs,Ph,Ps_hi,Acc).

	*/


/*
%!	node_production(+Node_corpus,+Branch_heads,+Productions,+Node_head_production,-Expanded_productions,-Node_production) is nondet.
%
%	Implementation of v. 2.2.0 production-composition rules.
%	Currently made terribly, eye-wateringly explicit to make it
%	perfectly clear and easier to debug/refactor.
%
%	@TODO: refactor to make more declarative.
%
node_production(Cs,Bs,Ps,Ph,Ps_,Ph_i):-
	% Many examples and more than one branch indicate a branch node.
	length(Cs, Cs_l)
	,length(Bs, Bs_l)
	,Cs_l > 1
	,Bs_l > 1
	,[Hi|_] = Bs
	% Create a new node production
	,Ph_i = (Hi --> [Hi])
	% augment current node-head production
	% with a nonterminal reference to Hi.
	,augmented_node_head_production(Ph, Hi, A_Ph)
	% Add the augmented node-head production to known Productions set
	,expanded_productions(Ps,[],A_Ph,Ps_).

node_production(Cs,Bs,Ps,Ph,Ps_,Ph_i):-
	% Many examples for a single branch, indicating a branch node.
	length(Cs, Cs_l)
	,length(Bs, Bs_l)
	,Cs_l > 1
	,Bs_l = 1
	,[Hi|_] = Bs
	% Create a new node production
	,Ph_i = (Hi --> [Hi])
	% augment current node-head production
	% with a nonterminal reference to Hi.
	,augmented_node_head_production(Ph, Hi, A_Ph)
	% Add the augmented node-head production to known Productions set
	,expanded_productions(Ps,[],A_Ph,Ps_).

node_production(Cs,Bs,Ps,Ph,Ps_,Ph_i):-
	% C? is a single-token example, indicating a leaf node.
	length(Cs, 1)
	,Cs = [_C]
	,[Hi|_] = Bs
	% Augment Ph with a terminal Hi
	,augmented_node_head_production(Ph, [Hi], A_Ph)
	% Set the node-production to the augmented node-head production
	,Ph_i = A_Ph
	% Don't add it to the known productions yet (because it's not finished yet)
	,Ps_ = Ps.

node_production(Cs,Bs,Ps,Ph,Ps_,Ph_i):-
	% A single example with more than one tokens indicates a stem node.
	length(Cs, 1)
	,member(C, Cs)
	,length(C, C_l)
	,C_l > 1
	,[Hi|_] = Bs
	% Augment Ph with a terminal Hi
	,augmented_node_head_production(Ph, [Hi], A_Ph)
	% Set the node-production to the augmented node-head production
	,Ph_i = A_Ph
	% Don't add it to the known productions yet (because it's not finished yet)
	,Ps_ = Ps.



	*/
