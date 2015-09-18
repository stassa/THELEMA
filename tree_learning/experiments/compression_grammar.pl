:-module(compression_grammar, [so_ability//0, fo_ability//0]).

so_ability --> [].

fo_ability-->destroy.
fo_ability-->[exile,target,creature].

creature-->[creature],[if,it,is,black].
creature-->[creature].

destroy-->[destroy],target.

target-->[target],creature.
target-->[target,artifact].



/*
so_ability --> destroy_X.

destroy_X --> destroy, 'X'.

fo_ability-->destroy.

destroy-->[destroy], target.
destroy-->[destroy], 'X'.

'X'-->['X'], target.

target-->[target], artifact.
target-->[target, creature].
target-->[target], artifacts.

artifact-->[artifact], and.
artifact-->[artifact].

artifacts-->[artifacts], and.

and-->[and, enchantments].
*/

/*

so_ability-->destroy, target.
so_ability--> destroy_X_target, creatures_and_enchantments.

fo_ability --> destroy_X_target.
fo_ability --> creatures_and_enchantments.

destroy_X_target --> destroy, 'X_target'.
creatures_and_enchantments --> creatures, and_enchantments.


destroy-->[destroy].

target-->[target, artifact].
target-->[target, creature].

'X_target'-->['X', target].
creatures-->[creatures].
and_enchantments-->[and, enchantments].

*/





/*
so_ability --> destroy_X.

destroy_X --> destroy.

ability --> destroy.

destroy --> [destroy], 'X'.
destroy --> [destroy], target.

'X' --> ['X'], target.

target --> [target], artifacts.
target --> [target, creatures].
target --> [target, artifact].
target --> [target, creature].

artifacts --> [artifacts], and.
and --> [and, enchantments].
*/
