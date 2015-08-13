:-module(compressed_corpus_grammar, [ability//1]).

ability(destroy) --> t_destroy, all_artifacts, t_and, t_enchantments.
ability(destroy) --> t_destroy, all_artifacts, t_creatures, t_and, t_enchantments.
ability(destroy) --> t_destroy, all_artifacts, t_creatures, t_and, t_lands.
ability(destroy) --> t_destroy, all_artifacts.
ability(destroy) --> t_destroy, any_number_of_target, t_artifacts, [and/or], t_enchantments.
ability(destroy) --> t_destroy, any_number_of_target, t_creatures.
ability(destroy) --> t_destroy, each_artifact, with_converted_mana_cost_X_or_less.
ability(destroy) --> t_destroy, each_artifact, creature_and_enchantment_with_converted_mana_cost_X.
ability(destroy) --> t_destroy, six_target_creatures.
ability(destroy) --> t_destroy, t_target, t_artifact, t_and, all_other_artifacts_with_the_same_name_as_that_artifact.
ability(destroy) --> t_destroy, t_target, t_artifact, t_and, t_target, t_enchantment.
ability(destroy) --> t_destroy, t_target, t_artifact, creature_or_black_creature.
ability(destroy) --> t_destroy, t_target, white_creature.
ability(destroy) --> t_destroy, three_target_permanents.
ability(destroy) --> t_destroy, two_target, t_artifacts.
ability(destroy) --> t_destroy, two_target, 'nonblack_creatures_unless_either_one_is_a_color_the_other_isn\'t'.
ability(destroy) --> t_destroy, 'X_target', t_artifacts.
ability(destroy) --> t_destroy, 'X_target', snow_lands.

snow_lands-->[snow, lands].
'X_target'-->['X', target].
'nonblack_creatures_unless_either_one_is_a_color_the_other_isn\'t'-->[nonblack, creatures, unless, either, one, is, a, color, the, other, 'isn\'t'].
t_artifacts-->[artifacts].
two_target-->[two, target].
three_target_permanents-->[three, target, permanents].
white_creature-->[white, creature].
creature_or_black_creature-->[creature, or, black, creature].
t_enchantment-->[enchantment].
all_other_artifacts_with_the_same_name_as_that_artifact-->[all, other, artifacts, with, the, same, name, as, that, artifact].
t_and-->[and].
t_artifact-->[artifact].
t_target-->[target].
six_target_creatures-->[six, target, creatures].
creature_and_enchantment_with_converted_mana_cost_X-->[creature, and, enchantment, with, converted, mana, cost, 'X'].
with_converted_mana_cost_X_or_less-->[with, converted, mana, cost, 'X', or, less].
each_artifact-->[each, artifact].
t_creatures-->[creatures].
artifacts_and_or_enchantments-->[artifacts, and/or, enchantments].
any_number_of_target-->[any, number, of, target].
t_lands-->[lands].
t_enchantments-->[enchantments].
creatures_and-->[creatures, and].
and_enchantments-->[and, enchantments].
all_artifacts-->[all, artifacts].
t_destroy-->[destroy].

