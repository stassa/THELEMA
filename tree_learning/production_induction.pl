:-module(production_induction, [corpus_productions/2]).

:-use_module(configuration).
:-use_module(project_root(utilities)).


%!	corpus_productions(+Corpus:list, -Productions:list) is det.
%
%	Derive a set of grammar Productions from the given Corpus.
%
%	Corpus is a list of lists where each sub-list is a tokenised
%	example in the target language. Productions is a list of grammar
%	productions in DCG notation.
%
corpus_productions(Cs, Ps_):-
	node_heads(Cs, Bs)
	,start_production(Ph)
	,derived_productions(Cs,Bs,Ph,Ps)
	,sort(Ps, Ps_).


%!	derived_productions(+Corpus,+Node_heads,+Node_production,-Productions) is det.
%
%	Derive a set of grammar Productions from Corpus. Node_heads is
%	the set of heads of nodes under the currently examined node, as
%	a list of atoms. Node_production is the node-head production for
%	the current node.
%
derived_productions(Cs, Bs, Ph, Ps):-
	% Note: we build up the third argument as a stack beginning here.
	derived_productions(Cs, Bs, [Ph], [], Ps).


%!	derived_productions(+Corpus,+Node_heads,+Node_production,+Temp,-Acc) is det.
%
%	Business end of derived_productions/4.
%
%	@TODO: Document; particularly the bit where we build a stack of
%	node-head productions.
%
derived_productions([],_,_,Ps,Ps).

derived_productions([[_C]],[Hi],[Ph|_],Ps,[A_Ph|Ps]):-
	% A leaf node (single, single token example, single branch)
	you_are_here(1),
	augmented_node_head_production(Ph, [Hi], A_Ph).

derived_productions([[C|Cs]],[_Hi],[Ph|_],Ps,[Ph,A_Ph|Ps]):-
	you_are_here(2),
	% A stem node (single example, single branch)
	augmented_node_head_production(Ph, [C|Cs], A_Ph).

derived_productions([C|Cs],[Hi],[Ph|[Ph_Prev|Ph_s]],Ps,Acc):-
	% A branch node (multiple examples for a single branch)
	you_are_here(3)
	,configuration:lexicalisation_strategy(S)
	,S \= none
	,node_head_production(Hi, Ph_i)
	,augmented_node_head_production(Ph, Hi, A_Ph)

	% Lexicalisation- augmentation; hopefully.
	,parameterised_node_head_production(Ph_Prev,Hi,L_Ph_Prev)

	,beheaded_node_corpus([C|Cs],B_Cs)
	,node_heads(B_Cs, Bs_Hs)
	,derived_productions(B_Cs, Bs_Hs, [Ph_i,A_Ph|Ph_s], [A_Ph,L_Ph_Prev|Ps], Acc).

derived_productions([C|Cs],[Hi],[Ph|Ph_s],Ps,Acc):-
	% A branch node (multiple examples for a single branch)
	you_are_here(3)
	,node_head_production(Hi, Ph_i)
	,augmented_node_head_production(Ph, Hi, A_Ph)
	,beheaded_node_corpus([C|Cs],B_Cs)
	,node_heads(B_Cs, Bs_Hs)
	,derived_productions(B_Cs, Bs_Hs, [Ph_i,A_Ph|Ph_s], [A_Ph|Ps], Acc).


derived_productions(Cs,[Hi|Bs],[Ph|Ph_s],Ps,Acc):-
	you_are_here(4),
	% Softcuts avoid unproductive backtracking
	% when beheaded node-corpus is not []
	once(augmented_node_head_production(Ph, [Hi], A_Ph))
	,once(split_corpus(Hi,Cs,Cs_hi,Cs_Rest))
	,beheaded_node_corpus(Cs_hi,[])
	% We continue with the rest-of-corpus and so drop the A_Ph
	% from the node-production stack, else it will replace the one
	% We actually need.
	,derived_productions(Cs_Rest,Bs,[Ph|Ph_s],[A_Ph|Ps],Acc).

derived_productions(Cs,[Hi|Bs],[Ph|Ph_s],Ps,Acc):-
	you_are_here(5),
	node_head_production(Hi, Ph_i)
	,augmented_node_head_production(Ph, Hi, A_Ph)
	,split_corpus(Hi,Cs,Cs_hi,Cs_Rest)
	,beheaded_node_corpus(Cs_hi,B_Cs_hi)
	,node_heads(B_Cs_hi,Bs_hi)
	,derived_productions(B_Cs_hi,Bs_hi,[Ph_i,A_Ph|Ph_s],[A_Ph|Ps],Ps_hi)
	,you_are_here(6)
	,derived_productions(Cs_Rest,Bs,[Ph|Ph_s],Ps_hi,Acc).

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


%!	node_head_productions(+Node_head, -Production) is semidet.
%
%	Construct the node-head Production for the given Node_head.
%
node_head_production(Hi, Ph_i):-
	configuration:production_composition(S)
	,node_head_production(S, Hi, Ph_i).


%!	node_head_production(+Strategy, +Node_head, -Production) is semidet.
%
%	Business end of node_head_production/2. Strategy is the value of
%	configuration option production_composition/1.
%
node_head_production(basic, Hi, (Hi --> [Hi])).


%!	augmented_node_head_production(+Node_head_production,+Token,-Augmented_branch_production) is det.
%
%	Augment Branch_production with Token.
%
%	Token is the head of a new child node in the currently expanding
%	node and can be either a single-element list, or an atom.
%
%	A single-element list indicates a literal token, to be added as
%	a terminal to the right-hand side of Node_head_production,
%	whereas an atom indicates a reference to a newly created
%	node-head production, to be added as a nonterminal to the rhs of
%	Node_head_production.
%
%	With lexicalisation_strategy/1 options "branch", "token" and
%	"tokens" a number of lexical parameters will also be added to
%	the heads of rules.
%
augmented_node_head_production(Ph, Hi, A_Ph):-
	configuration:lexicalisation_strategy(S)
	,augmented_node_head_production(S,Ph,Hi,A_Ph).


%!	augmented_node_head_production(+Strategy,+Node_head_production,+Token,-Augmented_branch_production) is det.
%
%	Business end of augmented_branch_production/3; Strategy is the
%	value of configuration option lexicalisation_strategy/1.
%
augmented_node_head_production(_, Ph --> [] , H, (Ph --> H)).

augmented_node_head_production(S, Ph --> [T] , [H], (Ph --> [T,H])):-
	% A leaf! We have only the rule-synonym token and we're adding
	% a single terminal to it. So it's a leaf and we're about to close it.
	(   S = none
	;   S = branch
	).
augmented_node_head_production(token, Ph --> [T] , [H], (A_Ph --> [T,H])):-
	% A leaf
	A_Ph =.. [Ph,H].

augmented_node_head_production(none, Ph --> [T] , H, (Ph --> [T],H)):-
	% A branch from a brand new node-head production
	% Not checking whether H is atomic with atomic/1
	% because of tokens like 'and/or' etc that are not (atomic).
	\+ is_list(H).
augmented_node_head_production(S, Ph --> [T] , H, (A_Ph --> [T],H)):-
	% A branch from a brand new node-head production
	(   S = branch
	;   S = token
	)
	,\+ is_list(H)
	,A_Ph =.. [Ph,H].

augmented_node_head_production(S, Ph --> B , [H], (Ph --> Bs_t)):-
	% A leaf
	(   S = none
	;   S = branch
	)
	,tree_list(B, Bs)
	,append(Bs, [[H]], Bs_)
	,list_tree(Bs_, Bs_t).
augmented_node_head_production(token, Ph --> B , [H], (A_Ph --> Bs_t)):-
	% A leaf
	tree_list(B, Bs)
	,append(Bs, [[H]], Bs_)
	,list_tree(Bs_, Bs_t)
	,A_Ph =.. [Ph,H].

augmented_node_head_production(none, Ph --> B , H, (Ph --> Bs_t)):-
	% A branch for a previously augmented production
	\+ is_list(H)
	,tree_list(B, Bs)
	,append(Bs, [H], Bs_)
	,list_tree(Bs_, Bs_t).
augmented_node_head_production(S, Ph --> B , H, (A_Ph --> Bs_t)):-
	% A branch for a previously augmented production
	(   S = branch
	;   S = token
	)
	,\+ is_list(H)
	,tree_list(B, Bs)
	,append(Bs, [H], Bs_)
	,list_tree(Bs_, Bs_t)
	,A_Ph =.. [Ph,H].

augmented_node_head_production(S, Ph --> B , H, (Ph --> Bs_t)):-
	% A stem
	(   S = none
	;   S = branch
	)
	,is_list(H)
	,tree_list(B, Bs)
	,append(Bs, [H], Bs_)
	,list_tree(Bs_, Bs_t).
augmented_node_head_production(token, Ph --> B , [H|T], (A_Ph --> Bs_t)):-
	% A stem
	%is_list(H)
	tree_list(B, Bs)
	,append(Bs, [[H|T]], Bs_)
	,list_tree(Bs_, Bs_t)
	,A_Ph =.. [Ph,H].



%!	parameterised_node_head_production(+Production,+Node_head,-Lexicalised) is semidet.
%
%	Lexicalised is the given Production with Node_head as a lexical
%	parameter. For example:
%
%	==
%	parameterised_node_head_production((destroy-->target(creature))
%					  ,target
%					  ,destroy(target)-->target(creature)
%       ==
%
parameterised_node_head_production(Ph_Prev --> B, Hi, Ph_Prev --> Bs_t):-
	tree_list(B, Bs)
	,reverse(Bs, [T|Sb])
	,T_L =.. [T,Hi]
	,reverse([T_L|Sb], Bs_L)
	,list_tree(Bs_L, Bs_t).


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
