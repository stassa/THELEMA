:-module(rule_machine, [transition/5
		       ,transitions/5
		       ,list_of/3]).

:- dynamic state/3.
:- multifile state/3.

%!	state(?Type,?Value,?Index) is det.
%
%	A rule machine state. In this module, only the index of invalid
%	states, -1 is defined.
%
%	Hackity hack. There's a few things that need to happen all at
%	once for this clause to make any sense.
%	1) This module needs to load _after_ any module that imports it
%	as an additional import module (ie, after a rule engine module,
%	eg, mtg_rule_engine).
%	2) No other state must have an index of -1 (ie, all states
%	indices must be 0 or positive
%	3) transition/5 must check that a transition produces a valid
%	(ie 0 or positive) index.
%	4) Any new states added to the database must be added to the
%	start of the state/3 er, block, using asserta/1 and never
%	assertz/1.
%
%	The point of all of the above is so that it ends up at the end
%	of the state/3 clause block. If it's ever at the start or before
%	any other clause, obviously, it will bind to any state/3 term
%	and short-circuit clause selection.
%
%	So it's a bit of a hack (to say the least) and the whole point
%	of it is to bind an invalid-state term to the output of
%	transition/5. Oh well. I'll probably regret it at some point.
%
%	ToDO: right, never mind- this is not going to work. At all.
%	Ever.
%	Because to declare state/3 as multifile you need to load it
%	before any other module that declares it also. And there's no
%	use trying to declare as multifile in other modules- for some
%	reason. Not sure. It's not documented! :(
%
%state(_, _, -1).


%!	transition(?State:atom,+Current:may_be(atom,number),+Distance:number,+Direction:term,-New_state) is nondet.
%
%	Advance State by the given Distance towards Direction, to find a
%	New_state.
%
%	State is the name of a state/3 relation, for example:
%	  state(t, tapped, 1).
%         state(t, untapped, 0).
%
%	If the second argument of state/3 would be numeric, ie if the
%	state is a numeric state rather than a symbolic category, then
%	the mapping is implicitly:
%         state(S, N, N).
%
%	In other words a numeric state will be modified to a new state
%	by simple arithmetic.
%
%	If the second argument of state/3 is symbolic, the new state is
%	the state that is at a distance equal to Distance from the
%	current State in the given Direction.
%
%	Direction must be an arithmetic fuction with exactly two
%	operands, such as +,-,/,*,div,gcd,rdiv,xor etc. This function is
%	applied to Current and Distance, to yield New_State
%
%	transition/5 makes no assumptions about the ordering of states.
%	In other words, if you pass a garbage Distance, Direction, or
%	current State, you'll get out a garbage New_state, or
%	transition/5 will fail:
%         ?- transition(t, tapped, 1, +, New_state).
%         false.
%
transition(_State, Current, Distance, Direction, New_state):-
	number(Current) % State is a number: its mapping to Z yields itself.
	,Transition =.. [Direction,Current,Distance]
	,New_state is Transition
	, New_state >= 0
	,!. % And stop here.
transition(State, Current, Distance, Direction, New_state):-
	state(State,Current,Index)
	,Operation =.. [Direction,Index,Distance]
	,New_index is Operation
	,New_index >= 0
	,state(State,New_state,New_index).
transition(_, _, _, _, invalid_state(invalid_rule_machine_state)).

%!	transitions(+State,+Currents,+Distances,+Directions,-New_state) is nondet.
%
%	As transition/5 but Currents Distances and Directions are lists
%	rather than single terms and State is a state/n term where n an
%	arbitrarily large number. state/n defines a state with n
%	parameters, for example:
%
%	  state(zone, Object, Zone, Player, Index).
%
%	  (An Object in the given Zone on the side of a given Player)
%	  TODO: not quite- since this calls transition/5, state/2 will
%	  be called so we won't be looking for multi-parameter states.
%
%	Currents is a list of parameters of the current state (Object,
%	Zone, Player in the example).
%
%	Distances and Directions must be at least 1 and at most L in
%	length where L is the length of Currents.
%
%	Each number in Distances is used to modify the corresponding
%	parameter of States, according to the corresponding element of
%	Direction. "Corresponding" here is used to mean "with the same
%	index in the respective list".
%
%	If Distances is a single-element list each Current state is
%	modified by that number.
%
%	If Directions is a single-element list, each Current state is
%	modified by that operation.
%
%	New_state is the state we find by moving the current state's
%	parameters by the given Distances and Directions. In short, this
%	is a move in n-dimensional space, where n the dimensions of
%	Currents.
transitions(State,Currents,[Distance],[Direction],New_state):-
	length(Currents, L)
	,list_of(L, Direction, Directions)
	,list_of(L, Distance, Distances)
	,transitions(State,Currents,Distances,Directions,New_state).
transitions(State,Currents,[Distance],Directions,New_state):-
	length(Currents, L)
	,list_of(L, Distance, Distances)
	,transitions(State,Currents,Distances,Directions,New_state).
transitions(State,Currents,Distances,[Direction],New_state):-
	length(Currents, L)
	,list_of(L, Direction, Directions)
	,transitions(State,Currents,Distances,Directions,New_state).
transitions(State,Currents,Distances,Directions,New_state):-
	transitions(State,Currents,Distances,Directions,[],New_state).

/*
Some examples:

- Untap all, with a single distance & direction:

?- transitions(t, [tapped,untapped], [1], [>>], New).
New = [untapped, untapped] .


- Tap all, single direction and multiple distances:

?- transitions(t, [tapped,untapped], [0,1], [+], New).
New = [tapped, tapped] .


- Tap select objects with a single direction and multiple distances

?- transitions(t, [untapped,untapped,untapped,untapped], [1], [+,+,*,*], New).
New = [tapped, tapped, untapped, untapped] .


- Tap select objects, with multiple directions and distances:

?- transitions(t, [untapped,untapped,untapped,untapped], [0,1,1,1], [+], New).
New = [untapped, tapped, tapped, tapped] .

	*/



%!	transitions(+S,+Cs,+Ds,+Vs,+Temp,-Acc) is nondet.
%
%	Business end of transitions/5.
transitions(_,[],[],[],Etats_wen, New_state):-
	reverse(Etats_wen, New_state).
transitions(State,[C|Cs],[D|Ds],[V|Vs],Temp, Acc):-
	transition(State,C,D,V,New)
	,transitions(State, Cs, Ds, Vs, [New|Temp], Acc).


%!	list_of(+Length, +Value, -List) is det.
%
%	Instantiate each element of a List with length Length to Value.
%
%	TODO: prime candidate for library predicate.
list_of(Length, Value, Instantiated):-
	length(Ls, Length)
	,findall(El
		,(member(El, Ls)
		 ,El = Value)
		,Instantiated)
	,!. %Greenery

/*
Examples:

- Untap all (where 0-states are don't-care)

?- notrace, transitions(t, [tapped,untapped], [1,0], [-,-], New).
New = [untapped, untapped] .

- Also untap all (where don't-care states are defined in Directions)

?- notrace, transitions(t, [tapped,untapped], [1,1], [-,*], New).
New = [untapped, untapped] .

- Tap all:

?- notrace, transitions(t, [tapped,untapped], [0,1], [+,+], New).
New = [tapped, tapped] .

	*/

% Re-defined in mtg_rule_machine module.
%state(t,tapped,1).
%state(t,untapped,0).


/*
Here's how to modify "tapped" to "untapped" as a symbolic resource:
1. Define a mapping: t:{tapped,untapped} -> {1,0}
  state(t,tapped,1).
  state(t,untapped,0).

2. Modify the numeric value of the Resource:
  ... call: modify(tapped, 1, -, S').
  ... call: state(t,tapped,1) ...
  ... call: 0 is 1 - 1 ...
  ... call: state(t,untapped, 0) ...
  ... call: S' = untapped ...

3. Return the symbolic value of the modified Resource.
  ... exit: modify(tapped, 1, -, untapped) ...


This way you can deal with multi-modal symbolic resources (with any
number of states that is) in a high-level manner, without having to
determine state transitions for each and every one of them.

Obviously you need to be careful with the Amount of the modification but
that is up to the user of the modify instruction.

Note also that the state/3 predicates define a direction from a state to
another- for example, in the t-state example above, the ordering of the
t set follows the ordering of R, ie tapped, untapped are ordered
according to 1,0. Which means also that to go from tapped to untapped
you need to pass the relation that would take youf rom 1 to 0- in other
words, -.

In short, it's a monotonic relation, 'k?

OK, but now the question is: how do you represent numeric categories?
For example, what about life, the universe etc?

First of all we remark that:
  state(numeric, N, N).

i.e. if N is numeric, its mapping to Z yields itself.

Which means there's two ways to represent this:

1. With an explicit rule:
  state(_S, N, N):- number(N), !.

2. With an implicit rule, implemented client-side:

client_predicate(S, N, Result):-
	number(N)
	, ... some numeric manipulation ...
client_predicate(S, N, Result):-
	\+ number(N)
	,state(S, N, V)
	, ... some symbolic manipulation ...

The explicit way is clearer, but I remember having trouble with
transitive closures of states defined as rules, and I fear this was the
case even when using cuts, so for security, let's leave the mapping
implicit and expect clients to sort it out.

Because I'd like to determine reachability between states at some point,
potentially and I'd like to do that without going infinite if
at all possible. I think keeping state/3 as facts-only will help, but
maybe I don't need to, which would be nice actually.

		   */











