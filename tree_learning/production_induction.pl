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
%derived_productions([],[_],Phs,Ps,Ps_):-
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

derived_productions(Cs,[Hi|Bs],[Ph|Phs],Ps,Acc):-
	% Multiple branches and an unsplit corpus
	% Split the corpus and follow each branch separately
	you_are_here(split),
	split_corpus(Hi,Cs,Cs_hi,Cs_Rest)
	,derived_productions(Cs_hi,[Hi],[Ph|Phs],Ps,Ps_hi)
	,you_are_here(split_2)
%	,derived_productions(Cs_Rest,Bs,[Ph],Ps_hi,Acc).
	,derived_productions(Cs_Rest,Bs,[Ph|Phs],Ps_hi,Acc).

you_are_here(_).


lexicalised_productions(Ps, Ps_l):-
	configuration:lexicalisation_strategy(S)
	,lexicalised_productions(S, [epsilon|Ps], [], Ps_l).

lexicalised_productions(none, [epsilon|Ps], [], Ps).

lexicalised_productions(greibach, [_P], Ps, Ps).
%lexicalised_productions(greibach, [P_lex,P,P|Ps], Temp, Acc):-
%	% Skip identical productions.
%	lexicalised_productions(greibach, [P_lex,P|Ps], Temp, Acc).
lexicalised_productions(greibach, [P_ref,P|Ps], Temp, Acc):-
	lexicalised_production(P, P_ref, P_lex)
	,lexicalised_productions(greibach, [P_lex|Ps], [P_lex|Temp], Acc).

lexicalised_production(P --> [T], epsilon, P_ --> [T]):-
	P_ =.. [P|[epsilon]].
lexicalised_production((P --> [T], N), Pi --> _, (P_-->[T],Pi)):-
	P_ =.. [P|[N]].
lexicalised_production(S --> _P, P_lex --> _, S --> P_lex):-
	phrase(configuration:start, [S]).



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


/*
%!	lexicalised_production(+Production,+Parameter,-Lexicalised) is det.
%
%	Parameterise Production with a lexical argument to produce its
%	Lexicalised form, according to the lexicalisation_strategy
%	option.
%
lexicalised_production(Production, Parameter, Lexicalised):-
	configuration:lexicalisation_strategy(S)
	,once(lexicalised_production(S,Production,Parameter,Lexicalised)).


%!	lexicalised_production(+Strategy,+Production,+Parameter,-Lexicalised) is semidet.
%
%	Business end of lexicalised_production/2. Clauses are selected
%	depending on the value of configuration option
%	lexicalisation_strategy/1.
%
lexicalised_production(none, P, _, P).

lexicalised_production(greibach, S --> [], _, S --> []).
lexicalised_production(greibach, S --> N, P --> _B, S --> P):-
% S is the start symbol expanding to a nonterminal N, P is a compound
% with at least one argument, ie it's lexicalised and N is the functor
% name of the lexicalised P, ie in the body of S//0 N is a reference to
% P.
	phrase(configuration:start, [S])
	,\+ lexicalised(N, _, _)
	,lexicalised(P, N, _).
lexicalised_production(greibach, S --> N, P --> _, S --> N):-
	phrase(configuration:start, [S])
	,\+ lexicalised(N,_,_)
	,\+ lexicalised(P, _, _).
lexicalised_production(greibach,(P --> [T], N), N --> _, (P_ --> [T], N)):-
	\+ lexicalised(P, _, _)
	,\+ lexicalised(N, _, _)
	,P_ =.. [P,N].
lexicalised_production(greibach,(P --> [T], N), Ph_i --> _, (P --> [T], Ph_i)):-
	\+ lexicalised(N, _, _)
	,lexicalised(Ph_i, N, _).
lexicalised_production(greibach, P --> [P], [], P_ --> [P]):-
	P_ =.. [P,epsilon].

lexicalised_production(greibach, (P --> [T], N) ,(P --> [T], N)
		      ,(P --> [T], N_)):-
	you_are_here(1)
	,lexicalised(P, _, [N|_])
	,\+lexicalised(N, _, _)
	,N_ =.. [N,epsilon].

%lexicalised_production(greibach, P, P, P):-
%	writeln(not_lexicalising:P).


%!	lexicalised(+Term,-Symbol,-Parameters) is nondet.
%
%	True when Term either the left-hand side symbol or a right-hand
%	side constituent of a DCG rule and it has at least one lexical
%	parameter. Symbol is the functor name of the lexicalised
%	constituent and Parameters the list of its arguments.
%
%	Just a thin shell around uneef (=../2) to make explicit the
%	semantics of its use in this context (to determine whether a
%	term is lexicalised or not).
%
lexicalised(T, F, [Args|Rest]):-
	T =.. [F|[Args|Rest]].
*/

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
sanitise_constituent(Pf, T, T_):-
	T =.. [F|As]
	,safe_identifier(F, F_)
	,A =.. [F_|As]
	,dcg_translate_rule(A --> [], H:-_)
	,(   predicate_property(H, built_in)
	 ->  atom_concat(Pf, F_, N_F)
	    ,T_ =.. [N_F|As]
	 ;   T_ = A
	 ).


%!	safe_identifier(+Identifier, -Safe) is det.
%
%	Ensure Identifier is safe to use as a nonterminal symbol. If
%	true, Safe is bound to Identifier. Otherwise it is bound to a
%	a configured safe term.
%
safe_identifier(I, S):-
	atomic_identifier(I, A)
	,valid_nonterminal(A, S).

atomic_identifier(I, A):-
        % Ensure production head is an atom
	(   \+ atom(I)
	->  term_to_atom(I, A)
	;   A = I
	).


%!	valid_nonterminal(+Nonterminal, -Valid) is det.
%
%	Ensure Nonterminal is a valid DCG rule left-hand side. If true,
%	Valid is bound to Nonterminal, otherwise it's bound to the term
%	mapped to Nonterminal in the configuration option
%	valid_nonterminal//1.
%
valid_nonterminal(N, N_):-
	phrase(configuration:valid_nonterminal(N_), [N])
	,! % No more results needed at this point.
	.
valid_nonterminal(N, N).


/*
In the process of removing duplicates; hope.

%derived_productions([],[],[Ph_i|_],Ps,[Ph_i|Ps]):-
%derived_productions([],[_],Phs,Ps,Ps_):-
	%lexicalised_productions(Phs, Phs_l)
	%,append(Phs_l, Ps, Ps_).
derived_productions([],[_],[_Ph|Phs],Ps,Ps_):-
	lexicalised_productions(Phs, Phs_l)
	,append(Phs_l, Ps, Ps_).

derived_productions([[_C]|Cs],[Hi],[Ph|Phs],Ps,Acc):-
	% A leaf node? (at least one single token example, single branch)
	you_are_here(leaf)
	,derived_production(Hi, Ph_i)
	,augmented_production(Ph, Hi, A_Ph)
%	,derived_productions(Cs,[Hi],[Ph,A_Ph|Phs],[Ph_i, A_Ph|Ps],Acc).
	,derived_productions(Cs,[Hi],[Ph,Ph_i,A_Ph|Phs],Ps,Acc).

derived_productions(Cs_hi, [Hi], [Ph|Phs], Ps, Acc):-
	% Single branch and its node-corpus.
	% We derive productions and go on with new branches
	you_are_here(branch )
	,derived_production(Hi, Ph_i)
	,augmented_production(Ph, Hi, A_Ph)
	,beheaded_node_corpus(Cs_hi, B_Cs_hi)
	,node_heads(B_Cs_hi, Bs_hi)
%	,derived_productions(B_Cs_hi,Bs_hi,[Ph_i,A_Ph|Phs],[A_Ph|Ps],Acc).
	,derived_productions(B_Cs_hi,Bs_hi,[Ph_i,A_Ph|Phs],Ps,Acc).

%derived_productions(Cs,[Hi|Bs],Phs,Ps,Acc):-
derived_productions(Cs,[Hi|Bs],[Ph|Phs],Ps,Acc):-
	% Multiple branches and an unsplit corpus
	% Split the corpus and follow each branch separately
	you_are_here(split),
	split_corpus(Hi,Cs,Cs_hi,Cs_Rest)
	,derived_productions(Cs_hi,[Hi],[Ph|Phs],Ps,Ps_hi)
	,you_are_here(split_2)
%	,derived_productions(Cs_Rest,Bs,Phs,Ps_hi,Acc).
	,derived_productions(Cs_Rest,Bs,[Ph],Ps_hi,Acc).

you_are_here(_).

*/

/*
%derived_productions([[_C]|Cs],[Hi],[Ph,Ph_g|Phs],Ps,Acc):-
derived_productions([[_C]|Cs],[Hi],Phs,Ps,Acc):-
	% A leaf node? (at least one single token example, single branch)
	you_are_here(leaf),
	(    Phs = [Ph] % Single element in the production ancestry stack.
	 ->  Ph_g = Ph  % Ancestor is current production.
	;    Phs = [Ph|[Ph_g|Phs_r]] % Else, there's a line of ancestors.
	 )
	,derived_production(Hi, Ph_i)
	,augmented_production(Ph, Hi, A_Ph)
	,lexicalised_production(A_Ph, Ph_i, A_Ph_l)
	,lexicalised_production(Ph_g, A_Ph_l, Ph_g_l)
	,derived_productions(Cs,[Hi],[Ph,A_Ph_l|Phs_r]
			    ,[Ph_i, A_Ph_l, Ph_g_l|Ps],Acc).

%derived_productions(Cs_hi, [Hi], [Ph,Ph_g|Phs], Ps, Acc):-
derived_productions(Cs_hi, [Hi], Phs, Ps, Acc):-
	% Single branch and its node-corpus.
	% We derive productions and go on with new branches
	you_are_here(branch ),
	(    Phs = [Ph]
	 ->  Ph_g = Ph
	;    Phs = [Ph|[Ph_g|Phs_r]]
	 )
	,derived_production(Hi, Ph_i)
	,augmented_production(Ph, Hi, A_Ph)
	,lexicalised_production(A_Ph, Ph_i, A_Ph_l)
	,lexicalised_production(Ph_g, A_Ph_l, Ph_g_l)
	,beheaded_node_corpus(Cs_hi, B_Cs_hi)
	,node_heads(B_Cs_hi, Bs_hi)
	,derived_productions(B_Cs_hi,Bs_hi,[Ph_i,A_Ph_l|Phs_r]
			    ,[Ph_g_l|Ps],Acc).

derived_productions(Cs,[Hi|Bs],Phs,Ps,Acc):-
	% Multiple branches and an unsplit corpus
	% Split the corpus and follow each branch separately
	you_are_here(split),
	split_corpus(Hi,Cs,Cs_hi,Cs_Rest)
	,derived_productions(Cs_hi,[Hi],Phs,Ps,Ps_hi)
	,you_are_here(split_2)
	,derived_productions(Cs_Rest,Bs,Phs,Ps_hi,Acc).

you_are_here(_).

*/

/*
lexicalised_production(greibach, S --> [], _, S --> []).
lexicalised_production(greibach, S --> N, P_hi --> _Hi, S --> P_hi):-
% If N is atomic, S is the start symbol and it's not yet lexicalised
	phrase(language:start, [S])
	,atomic(N).
lexicalised_production(greibach, P --> [T], P --> [T], P_ --> [T]):-
% P -->[T] is a leaf; it is followed by the empty string.
	P_ =.. [P|[epsilon]].
lexicalised_production(greibach, (P --> [T],N), (P --> [T],N) ,(P --> [T],N_)):-
	N_ =.. [N,epsilon]
	,!.
lexicalised_production(greibach, (P -->[T], N), N --> _B, ( P_ --> [T], N)):-
% If P is atomic it has not been lexicalised yet. If N is a constituent
% of P, it is probably lexicalised in which case update its reference in
% the head of P.
	atomic(P)
	,atomic(N)
	,P_ =.. [P|[N]].
lexicalised_production(greibach, (P -->[T], N), P_hi --> _B,( P --> [T], P_hi)):-
% If P is atomic it has not been lexicalised yet. If N is a constituent
% of P, it is probably lexicalised in which case update its reference in
% the head of P.
	compound(P)
	,atomic(N)
	,compound(P_hi)
	,P \= P_hi.

lexicalised_production(greibach, P, Ph, P):-
	writeln(not_lexicalising:P-with:Ph).
*/



/*
lexicalised_production(greibach, S --> N, Hi, S --> N_):-
% Only the S(tart symbol) expands to a single nonterminal.
% In that case we only want to lexicalise its single constituent.
% But only if it's not the empty string.
	atomic(N)
	,(   N \= []
	->  N_ =.. [N|[Hi]]
	;   N_ = N
	).
lexicalised_production(greibach, P --> ([T],N), Hi, P_ --> ([T],N)):-
	atomic(P)
	,P_ =.. [T|[Hi]].
lexicalised_production(_, P, _, P).

lexicalised_production(greibach, P --> ([T],N), Hi, P_ --> ([T],N)):-
	P =.. [T|As]
	,append(As, [Hi], As_)
	,P_ =.. [T|As_].

	*/

