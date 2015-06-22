:-module(mtg_rule_engine, [move/3
			  ,advance/3
			  ,modify/5
			  ,translate/3
			  ,mark/4]).
:-add_import_module(mtg_rule_engine, rule_machine, end).

:-dynamic object_state/3.

object_state(object_1, zone, hand).
object_state(object_1, object, card).

object_state(object_2, counters(+,1,+,1), 2).
object_state(object_2, facing, down).

% Yep, you need that- starting values come from cards but in game,
% each card-template becomes an instance eh?
object_state(object_2, power, 2).
object_state(object_2, toughness, 3).

object_state(object_2, object, card).


object_state(object_3, loyalty, 5).

object_state(object_4, slope, untapped).
object_state(object_4, rotation, flipped).

% Player states
% Player resources- modify numerically only.
player_state(player_1, life, 20).
player_state(player_2, life, 20).

% To win, add 1
player_state(player_1, win, 0).
player_state(player_2, win, 0).

player_state(player_1, mana(colourless), 0).
player_state(player_1, mana(white), 0).
player_state(player_1, mana(blue), 0).
player_state(player_1, mana(black), 0).
player_state(player_1, mana(red), 0).
player_state(player_1, mana(green), 0).

player_state(player_2, mana(colourless), 0).
player_state(player_2, mana(white), 0).
player_state(player_2, mana(blue), 0).
player_state(player_2, mana(black), 0).
player_state(player_2, mana(red), 0).
player_state(player_2, mana(green), 0).

% Max deck size
player_state(player_1, max(deck), 60).
player_state(player_2, max(deck), 60).

% Max hand size
player_state(player_1, max(hand), 60).
player_state(player_2, max(hand), 60).

% Current library size
player_state(player_1, cards_in(library), 60).
player_state(player_2, cards_in(library), 60).

% Current cards in hand
player_state(player_1, cards_in(hand), 60).
player_state(player_2, cards_in(hand), 60).


%!	move(+Object,+Zone,-New_state) is det.
%
%	Move Object from its current zone to Zone.
%
%	New_state is bound to an object_state/3 term with the object's
%	new zone-state: object_state(Object, zone, Zone)
%
%	If the Object is not in any zone (ie, it does not exist) it will
%	be created in Zone, effectively creating a token. move/3 does
%	not check that the type of object is valid for the destination
%	zone so it's perfectly possible to create an Instant token on
%	the Battlefield.
%
%	TODO: Shouldn't that actuall create an invalid_state?
%
%	Attempting to move an Object from its current zone back to that
%	zone will succeed, leaving Object where it is.
%
move(Object, Zone2, New_state):-
	object_state(Object, zone, Zone1)
	,rule_machine:state(zone, Zone1, Index1)
	,rule_machine:state(zone, Zone2, Index2)
	,index_from(Index1, Index2, Distance, Direction)
	,transition(zone, Zone1, Distance, Direction, New_zone)
	,New_state = object_state(Object, zone, New_zone)
	,!. %Definitely green
% If there is no such object in any zone, create it in the destination
% zone.
move(Object, Zone, Object_state):-
	rule_machine:state(zone, Zone, _Index)
	,Object_state = object_state(Object, zone, Zone)
	,!. %Green



%!	advance(+Segment,+Distance,+Direction,-New) is nondet.
%
%	Advance the turn to a different turn segment, either a phase or
%	a turn.
%
%	Distance is the number of turn segments to advance; Direction is
%	one of 'forwards', 'backwards'.
%
%	For example, to advance to the pre-combat Main phase from the
%	Beginning phase, call:
%	  advance(main(precombat), 1, forwards, N).
%
%	New is the name of the new Turn segment.
%
advance(Phase,New_phase,New_state):-
	rule_machine:state(phase, Phase, Index1)
	,rule_machine:state(phase, New_phase, Index2)
	,index_from(Index1, Index2, Distance, Direction)
	,transition(phase, Phase, Distance, Direction, New_phase)
	,New_state = game_state(turn_segment, phase, New_phase).
advance(Step,New_step,New_state):-
	rule_machine:state(step(Phase), Step, Index1)
	,rule_machine:state(step(Phase), New_step, Index2)
	,index_from(Index1, Index2, Distance, Direction)
	,transition(step(Phase), Step, Distance, Direction, New_step)
	,New_state = game_state(turn_segment, step(Phase), New_step).


%!	modify(+Player,+Resource,+Amount,+Operation,-New_Amount) is nondet.
%
%	Modify the given Resource by Amount. Operation is a binary
%	arithmetic operation such as +, -, * and so on. Normally only +
%	and - are ever used in M:tG.
%
%	Player is the name of a player in the game or the atom 'game',
%	to signify a game resource.
%
%	Er, such as? Damn, I know I had an example...
modify(Player, Resource, Amount, Operation, New_amount):-
	player_state(Player, Resource, Current)
	,transition(_, Current, Amount, Operation, New_amount).


%!	translate(+Object,+Translation,-New_state) is nondet.
%
%	Turn or flip the object, which is generally a card (players
%	tend to complain when flipped).
%
%	Translation is the type of translation to apply to the object
%	and can be one of: tap or flip.
%
%	Translation is normally a binary business: we flip or turn a
%	card sideways or upside-down, then go the other way. States that
%	represent these physical positions are also binary (their values
%	are in {1,0}).
%
%	This means that the result of a translation operation is always
%	that the card adopts the _opposite_ state of the state it is
%	currently in.
%
%       To clarify this, if we were to call, for example:
%        translate('Grizzly Bears', slope, New).
%
%	Then the Bears would become tapped if they were currently
%	untapped and _untap if they were currently tapped)_.
%
%	The same goes for flipped/unflipped and face up/ face down
%	cards- the current state will change to its opposite.
%
%	Since it's not possible to flip a card that is already flipped,
%	there is no need to define a specific translation direction when
%	calling translate/3. Obviously this means the caller is
%	responsible to call translate only when a translation is
%	actually needed.
%
translate(Object, Translation, New_state):-
	object_state(Object,Translation, Current_state)
	,rule_machine:state(Translation, Current_state, Index1)
	,rule_machine:state(Translation, Opposite_state, Index2)
	,! % Stops backtracking into flopping, after flipping.
	,index_from(Index1,Index2,Distance,Direction)
	,transition(Translation,Current_state,Distance,Direction
		   ,Opposite_state)
	,New_state = object_state(Object,Translation,Opposite_state).


%!	mark(+Object,+Counter,-New_state) is nondet.
%
%	Place a (possibly new) Counter on Object.
mark(Object, Action, Counter, New_state):-
	(   Action = add
	->  Direction = +
	;   Action = remove
	->  Direction = -
	)
	,object_state(Object, Counter, Current)
	,transition(_, Current, 1, Direction, New)
	,New_state = object_state(Object, Counter, New)
	,!. % If the object already has counters, it's sorted out now.
mark(Object, Action, Counter, New_state):-
	(   Action = add
	->  Direction = +
	;   Action = remove
	->  Direction = -
	)
	,transition(_, 0, 1, Direction, New)
	,New_state = object_state(Object, Counter, New).


%!	index_from(+I,+J,-Distance,-Direction) is det.
%
%	Used by engine primitives to determine the operation and amount
%	needed to index from I to J. I and J are indices, ie
%	numbers starting at _1_ (and _not_ 0). Distance is an integer
%	that can be positive or negative and Direction is one of: + or
%	-.
%index_from(I, J, -1, *):-
%	(   I = invalid_state
%	;   J = invalid_state
%	)
%	,!.
index_from(Index1, Index2, Distance, Direction):-
	Index1 < Index2
	,Distance is Index2 - Index1, Direction = (+)
	,!. % Green
index_from(Index1, Index2, Distance, Direction):-
	Index1 >= Index2 % For clarity
	,Distance is Index1 - Index2, Direction = (-).


%!	object_type(Object) is det.
%
%	True iff the given object is really an object type.
object_type(Object):-
	object_state(Object, object, Type)
	,rule_machine:state(object, Type, _Type_index)
	,!.


%!	state(?State,?State_value,?Number) is det.
%
%	Defines a mapping between a state-set S to the set of
%	positive and negative integers, Z. The relation between S and Z
%	is monotonic, ie the two sets follow the same ordering.
%
%	Note that S can be a number also- state/3 is most useful in
%	defining symbolic categories but it can be equally useful in
%	defining numeric mappings.
%
rule_machine:state(slope,tapped,1). % General object states
rule_machine:state(slope,untapped,0).

rule_machine:state(rotation,flipped,1).
rule_machine:state(rotation,unflipped,0).

rule_machine:state(facing,up,1).
rule_machine:state(facing,down,0).

% Modify like with resources.
% Also, can add more randomisers, eg die(10) or whatevs.
rule_machine:state(sample, coin(2), 0).
rule_machine:state(sample, die(6), 0).

% At state N, the leveler card has the abilities given in that
% striation. The actual level of a card instance will be stored in a
% game_object/3 term, e.g:
%  game_object('Beastbreaker of Bala Ged'-23, level, 3).
rule_machine:state(striation,upper,1).
rule_machine:state(striation,middle,2).
rule_machine:state(striation,lower,3).

% A double-faced card may be face up or face down, showing its front or
% back face resepctively; therefore, "face" and "facing" are different
% states.
rule_machine:state(face,front,1).
rule_machine:state(face,back,1).

rule_machine:state(counter,add,1).
rule_machine:state(counter,remove,0).

% Object types
rule_machine:state(object, ability, 1).
rule_machine:state(object, card, 2).
rule_machine:state(object, copy, 3).
rule_machine:state(object, token, 4).
rule_machine:state(object, spell, 5).
rule_machine:state(object, permanent, 6).
rule_machine:state(object, emblem, 7).

% Zones
% Todo: public/ revealed etc?
rule_machine:state(zone,library,1).
rule_machine:state(zone,hand,2).
rule_machine:state(zone,stack,3).
rule_machine:state(zone,battlefield,4).
rule_machine:state(zone,graveyard,5).
rule_machine:state(zone,exile,6).
rule_machine:state(zone,command,7).

rule_machine:state(phase,beginning,1).
rule_machine:state(phase,main(precombat),2).
rule_machine:state(phase,combat,3).
rule_machine:state(phase,main(postcombat),4).
rule_machine:state(phase,ending,5).

rule_machine:state(step(beginning),untap,1).
rule_machine:state(step(beginning),upkeep,2).
rule_machine:state(step(beginning),draw,3).

rule_machine:state(step(combat),beginning_of_combat,1).
rule_machine:state(step(combat),declare_attackers,2).
rule_machine:state(step(combat),declare_blockers,3).
rule_machine:state(step(combat),combat_damage,4).
rule_machine:state(step(combat),end_of_combat,5).

rule_machine:state(step(ending),end,1).
rule_machine:state(step(ending),cleanup,2).

% Player states
rule_machine:state(life,gain,1).
rule_machine:state(life,lose,2).

% When one player wins the game, the other player loses it implicitly.
% However, there are effects that directly declare "you win the game" or
% "you lose the game" so these should both be engine states.
rule_machine:state(win, true, 1).
rule_machine:state(win, false, 0).

rule_machine:state(loss, true, 1).
rule_machine:state(loss, false, 0).

rule_machine:state(mana, add, 1).
rule_machine:state(mana, remove, 2).

