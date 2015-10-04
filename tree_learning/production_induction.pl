:-module(production_induction, [corpus_productions/2]).

:-use_module(configuration).
:-use_module(project_root(utilities)).

/** <module> Induce the structure of a grammar from examples given as flat strings of tokens.

*/

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
	derived_productions(Cs, Bs, [Ph], [], Ps).


%!	derived_productions(+Corpus,+Node_heads,+Node_production,+Temp,-Acc) is det.
%
%	Business end of derived_productions/4.
%
derived_productions([],[_],[_Ph|Phs],Ps,Ps_):-
	lexicalised_productions(Phs, Phs_l)
	,append(Phs_l, Ps, Ps_).

derived_productions([[_C]|Cs],[Hi],[Ph|Phs],Ps,Acc):-
	% A leaf node? (at least one single token example, single branch)
	you_are_here(leaf)
	,derived_production(Hi, Ph_i)
	,augmented_production(Ph, Hi, A_Ph)
	,derived_productions(Cs,[Hi],[Ph,Ph_i,A_Ph|Phs],Ps,Acc).

derived_productions(Cs_hi, [Hi], [Ph|Phs], Ps, Acc):-
	% Single branch and its node-corpus.
	% We derive productions and go on with new branches
	you_are_here(branch )
	,derived_production(Hi, Ph_i)
	,augmented_production(Ph, Hi, A_Ph)
	,beheaded_node_corpus(Cs_hi, B_Cs_hi)
	,node_heads(B_Cs_hi, Bs_hi)
	,derived_productions(B_Cs_hi,Bs_hi,[Ph_i,A_Ph|Phs],Ps,Acc).

derived_productions(Cs,[Hi|Bs],Phs,Ps,Acc):-
	% Multiple branches and an unsplit corpus
	% Split the corpus and follow each branch separately
	you_are_here(split),
	split_corpus(Hi,Cs,Cs_hi,Cs_Rest)
	,derived_productions(Cs_hi,[Hi],Phs,Ps,Ps_hi)
	,you_are_here(split_2)
	,derived_productions(Cs_Rest,Bs,Phs,Ps_hi,Acc).

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
%	Business end of split_corpus/4.
split_corpus(_, [], Cs_H, Cs_H, Cs_Rest, Cs_Rest).
split_corpus(H, [[H|C]|Cs], Cs_H, Cs_H_Acc, Cs_Rest, Cs_Rest_Acc):-
	!, % Avoid backtracking into third clause head.
	split_corpus(H, Cs, [[H|C]|Cs_H], Cs_H_Acc, Cs_Rest, Cs_Rest_Acc).

split_corpus(H, [C|Cs], Cs_H, Cs_H_Acc, Cs_Rest, Cs_Rest_Acc):-
	split_corpus(H, Cs, Cs_H, Cs_H_Acc, [C|Cs_Rest], Cs_Rest_Acc).


%!	derived_productions(+Node_head, -Production) is semidet.
%
%	Construct the node-head Production for the given Node_head.
%
derived_production(Hi, Ph_i_):-
	configuration:production_composition(S)
	,derived_production(S, Hi, Ph_i)
	,sanitise_names(Ph_i, Ph_i_).


%!	derived_production(+Strategy, +Node_head, -Production) is semidet.
%
%	Business end of derived_production/2. Strategy is the value of
%	configuration option production_composition/1.
%
derived_production(basic, Hi, (Hi --> [Hi])).


%!	augmented_production(+Node_head_production,+Token,-Augmented_branch_production) is det.
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
augmented_production(Ph, Hi, A_Ph_):-
	configuration:production_augmentation(Pa)
	,augmented_production(Pa,Ph,Hi,A_Ph)
	,sanitise_names(A_Ph, A_Ph_).


%!	augmented_production(+Strategy,+Node_head_production,+Token,-Augmented_branch_production) is det.
%
%	Business end of augmented_branch_production/3; Strategy is the
%	value of configuration option production_augmentation/1 (which
%	currently only supports a single option).
%

/* Produce rules in Greibach Normal Form, ie:
  P --> [P], N.

Where P a single atom that matches the name of the production and
N a nonterminal reference or the empty atom [].

The exception is the set of productions that link the
grammar's start-symbol to each of the top-level nonterminals in the
grammar. In that case, we don't create the production as:

S --> [S], N.

And that's because we don't want the Start symbol in the stream when
parsing or generating.

*/
augmented_production(greibach, Ph --> [] , H, (Ph --> H)).
augmented_production(greibach, Ph --> [P] , [H], (Ph --> [P],H)).
augmented_production(greibach, Ph --> [P] , H, (Ph --> [P],H)).

/* Produce a non-hierarchical set of rules covering sentence chunks- ie, a chunker. */
augmented_production(literals, Ph --> [] , H, (Ph --> H)).
% Add a new terminal; we're probably at a leaf
augmented_production(literals, Ph --> [B] , [H], (Ph --> [B|[H]])).
% Add a list of terminals; we're probably at a stem
augmented_production(literals, Ph --> B , H, (Ph --> B_)):-
	is_list(H)
	,append(B, H, B_).
% Discard a nonterminal reference; we're probably at a branch.
% And because we want to keep only fragments of whole productions
% We don't want to point to the next element in a hierarchy.
augmented_production(literals, Ph --> B , _, (Ph --> B)).

/*  Produce a hierarchical grammar in loose (though GNF-like) form
 *  Original version - kind of broken after implementing GNF.
*/
augmented_production(tail, Ph --> [] , H, (Ph --> H)).
augmented_production(tail, Ph --> [T] , [H], (Ph --> [T,H])).
augmented_production(tail, Ph --> [T] , H, (Ph --> [T],H)):-
	% not a terminal; not checking with atomic/1
	% because of tokens like 'and/or' etc that are not (atomic).
	\+ is_list(H).
augmented_production(tail, Ph --> B , [H], (Ph --> Bs_t)):-
	tree_list(B, Bs)
	,append(Bs, [[H]], Bs_)
	,list_tree(Bs_, Bs_t).
augmented_production(tail, Ph --> B , H, (Ph --> Bs_t)):-
	% not a terminal
	\+ is_list(H)
	,tree_list(B, Bs)
	,append(Bs, [H], Bs_)
	,list_tree(Bs_, Bs_t).
augmented_production(tail, Ph --> B , H, (Ph --> Bs_t)):-
	% a string of terminals
	is_list(H)
	,tree_list(B, Bs)
	,append(Bs, [H], Bs_)
	,list_tree(Bs_, Bs_t).



%!	lexicalised_productions(+Productions,-Lexicalised) is det.
%
%	Parameterise each term in Productions with a lexical argument to
%	produce its Lexicalised form according to the
%	lexicalisation_strategy option.
%
%	@TODO: this takes a bit of documenting. Basically, what we do is
%	look up the stack and update each reference to a production with
%	its correct arity- if the referred production has arity n,
%	ensure that references to it also have arity n. But need to
%	explain the how.
%
lexicalised_productions(Ps, Ps_l):-
	configuration:lexicalisation_strategy(S)
	,lexicalised_productions(S, [epsilon|Ps], [], Ps_l).


%!	lexicalised_productions(+Strategy,+Productions,+Temp,-Acc) is det.
%
%	Business end of lexicalised_productions/2. Clauses are selected
%	depending on the value of configuratoin
%	option lexicalisation_strategy/1.
%
lexicalised_productions(none, [epsilon|Ps], [], Ps).
lexicalised_productions(greibach, [_P], Ps, Ps).
lexicalised_productions(greibach, [P_ref,P|Ps], Temp, Acc):-
	lexicalised_production(P, P_ref, P_lex)
	,lexicalised_productions(greibach, [P_lex|Ps], [P_lex|Temp], Acc).


%!	lexicalised_production(+Production,+Reference_production,-Lexicalised) is det.
%
%	Update all references to the Reference_production in the body
%	and the head of Production so that they have the correct arity.
%
%	@TODO: yeah, takes a bit more documenting.
%
lexicalised_production(P --> [T], _, P_ --> [T]):-
	P_ =.. [P|[epsilon]].
lexicalised_production((P --> [T], N), Pi --> _, (P_-->[T],Pi)):-
	P_ =.. [P|[N]].
lexicalised_production(S --> _P, P_lex --> _, S --> P_lex):-
	phrase(configuration:start, [S]).



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



%!	sanitise_names(+Production,-Renamed) is det.
%
%	Ensure new productions (and nonterminals referring to them) are
%	valid identifiers. Rename a production's head if it would
%	compile into a built-in at the DCG compiler; ensure that
%	production names are valid Prolog identifiers; and so on.
%
sanitise_names(P, P_):-
	configuration:rename_built_ins(B)
	,sanitise_names(B,P,P_).


%!	sanitise_names(+Bool,+Production,-Renamed) is det.
%
%	Business end of sanitise_names/2; clauses are selected
%	depending on the value of configuration
%	option rename_built_ins/1.
%
sanitise_names(false, Ph, Ph):-
	!. % Makes det.
sanitise_names(Pf, (Ph --> [T], N), (Ph --> [T], N_)):-
	% Sanitise a production's body
	sanitise_constituent(Pf, N, N_)
	,! % Also make det.
	.
sanitise_names(Pf, Ph --> B, Ph_ --> B):-
	% Sanitise a production's head
	sanitise_constituent(Pf, Ph, Ph_).


%!	sanitise_constituent(+Bool_or_prefix,+Term,-Renamed) is det.
%
%	Rename a constituent of a production that would otherwise be
%	mistaken for a built-in after translating its production to a
%	DCG. Convenience predicate to reduce boilerplate in clauses of
%	sanitise_names/3.
%
sanitise_constituent(_, T, A):-
% Ensure the Term is atomic and not say, a number, punctuation character
% or a compound like and/or, +1/+* etc etc.
	\+atom(T)
	,term_to_atom(T, A).
sanitise_constituent(Pf, T, T_):-
	T =.. [F|As]
	,dcg_translate_rule(T --> [], H:-_)
	,(   predicate_property(H, built_in)
	 ->  atom_concat(Pf, F, N_F)
	    ,T_ =.. [N_F|As]
	 ;   T_ = T
	 ).

