:-module(second_order_grammar, [first_order_phrase/3, second_order_phrase/2, start//0, nonterminal//0, terminal//0, t_t_destroy_all_artifacts_t_and_t_enchantments//0, t_t_destroy_all_artifacts_t_creatures_t_and_t_enchantments//0, t_t_destroy_all_artifacts_t_creatures_t_and_t_lands//0, t_t_destroy_all_artifacts//0, t_t_destroy_any_number_of_target_t_artifacts_and_or_t_enchantments//0, t_t_destroy_any_number_of_target_t_creatures//0, t_t_destroy_each_artifact_with_converted_mana_cost_X_or_less//0, t_t_destroy_each_artifact_creature_and_enchantment_with_converted_mana_cost_X//0, t_t_destroy_t_six_target_creatures//0, t_t_destroy_t_target_t_artifact_t_and_all_other_artifacts_with_the_same_name_as_that_artifact//0, t_t_destroy_t_target_t_artifact_t_and_t_target_t_enchantment//0, t_t_destroy_t_target_t_artifact_t_creature_or_black_creature//0, t_t_destroy_t_target_white_creature//0, t_t_destroy_t_three_target_permanents//0, t_t_destroy_two_target_t_artifacts//0, 't_t_destroy_two_target_nonblack_creatures_unless_either_one_is_a_color_the_other_isn\'t'//0, t_t_destroy_X_target_t_artifacts//0, t_t_destroy_X_target_snow_lands//0, ability//0]).

:-multifile[start//0, nonterminal//0, terminal//0].
:-use_module(first_order_grammar).

first_order_phrase(Rule, Derivation, Rest_of_sentence):-phrase(nonterminal, [Rule]), atom(Rule), phrase(Rule, Derivation, Rest_of_sentence).

second_order_phrase(Rule, Derivation):-phrase(nonterminal, [Rule]), atom(Rule), phrase(Rule, Second_order_rules), findall(D, (member(Second_order_rule, Second_order_rules), phrase(Second_order_rule, D)), Derivation).

start-->[ability].

terminal-->[['X_target_snow_lands']].
terminal-->[['X_target_t_artifacts']].
terminal-->[[any_number_of_target_t_artifacts_and_or_t_enchantments]].
terminal-->[[each_artifact_creature_and_enchantment_with_converted_mana_cost_X]].
terminal-->[[each_artifact_with_converted_mana_cost_X_or_less]].
terminal-->[[t_and_t_enchantments]].
terminal-->[[t_and_t_target_t_enchantment]].
terminal-->[[t_creature_or_black_creature]].
terminal-->[[t_creatures_t_and_t_enchantments]].
terminal-->[[t_creatures_t_and_t_lands]].
terminal-->[[t_six_target_creatures]].
terminal-->[[t_t_destroy]].
terminal-->[[t_target_t_artifact]].
terminal-->[[t_target_white_creature]].
terminal-->[[t_three_target_permanents]].
terminal-->[['two_target_nonblack_creatures_unless_either_one_is_a_color_the_other_isn\'t']].
terminal-->[[two_target_t_artifacts]].
terminal-->[[[all_artifacts]]].
terminal-->[[[all_other_artifacts_with_the_same_name_as_that_artifact]]].
terminal-->[[[any_number_of_target]]].
terminal-->[[[t_and]]].
terminal-->[[[t_creatures]]].

nonterminal-->[t_t_destroy_X_target_snow_lands].
nonterminal-->[t_t_destroy_X_target_t_artifacts].
nonterminal-->[t_t_destroy_all_artifacts].
nonterminal-->[t_t_destroy_all_artifacts_t_and_t_enchantments].
nonterminal-->[t_t_destroy_all_artifacts_t_creatures_t_and_t_enchantments].
nonterminal-->[t_t_destroy_all_artifacts_t_creatures_t_and_t_lands].
nonterminal-->[t_t_destroy_any_number_of_target_t_artifacts_and_or_t_enchantments].
nonterminal-->[t_t_destroy_any_number_of_target_t_creatures].
nonterminal-->[t_t_destroy_each_artifact_creature_and_enchantment_with_converted_mana_cost_X].
nonterminal-->[t_t_destroy_each_artifact_with_converted_mana_cost_X_or_less].
nonterminal-->[t_t_destroy_t_six_target_creatures].
nonterminal-->[t_t_destroy_t_target_t_artifact_t_and_all_other_artifacts_with_the_same_name_as_that_artifact].
nonterminal-->[t_t_destroy_t_target_t_artifact_t_and_t_target_t_enchantment].
nonterminal-->[t_t_destroy_t_target_t_artifact_t_creature_or_black_creature].
nonterminal-->[t_t_destroy_t_target_white_creature].
nonterminal-->[t_t_destroy_t_three_target_permanents].
nonterminal-->['t_t_destroy_two_target_nonblack_creatures_unless_either_one_is_a_color_the_other_isn\'t'].
nonterminal-->[t_t_destroy_two_target_t_artifacts].

ability-->t_t_destroy_X_target_snow_lands.
ability-->t_t_destroy_X_target_t_artifacts.
ability-->'t_t_destroy_two_target_nonblack_creatures_unless_either_one_is_a_color_the_other_isn\'t'.
ability-->t_t_destroy_two_target_t_artifacts.
ability-->t_t_destroy_t_three_target_permanents.
ability-->t_t_destroy_t_target_white_creature.
ability-->t_t_destroy_t_target_t_artifact_t_creature_or_black_creature.
ability-->t_t_destroy_t_target_t_artifact_t_and_t_target_t_enchantment.
ability-->t_t_destroy_t_target_t_artifact_t_and_all_other_artifacts_with_the_same_name_as_that_artifact.
ability-->t_t_destroy_t_six_target_creatures.
ability-->t_t_destroy_each_artifact_creature_and_enchantment_with_converted_mana_cost_X.
ability-->t_t_destroy_each_artifact_with_converted_mana_cost_X_or_less.
ability-->t_t_destroy_any_number_of_target_t_creatures.
ability-->t_t_destroy_any_number_of_target_t_artifacts_and_or_t_enchantments.
ability-->t_t_destroy_all_artifacts.
ability-->t_t_destroy_all_artifacts_t_creatures_t_and_t_lands.
ability-->t_t_destroy_all_artifacts_t_creatures_t_and_t_enchantments.
ability-->t_t_destroy_all_artifacts_t_and_t_enchantments.

t_t_destroy_X_target_snow_lands-->[t_t_destroy, 'X_target_snow_lands'].
t_t_destroy_X_target_t_artifacts-->[t_t_destroy, 'X_target_t_artifacts'].
't_t_destroy_two_target_nonblack_creatures_unless_either_one_is_a_color_the_other_isn\'t'-->[t_t_destroy, 'two_target_nonblack_creatures_unless_either_one_is_a_color_the_other_isn\'t'].
t_t_destroy_two_target_t_artifacts-->[t_t_destroy, two_target_t_artifacts].
t_t_destroy_t_three_target_permanents-->[t_t_destroy, t_three_target_permanents].
t_t_destroy_t_target_white_creature-->[t_t_destroy, t_target_white_creature].
t_t_destroy_t_target_t_artifact_t_creature_or_black_creature-->[t_t_destroy, t_target_t_artifact, t_creature_or_black_creature].
t_t_destroy_t_target_t_artifact_t_and_t_target_t_enchantment-->[t_t_destroy, t_target_t_artifact, t_and_t_target_t_enchantment].
t_t_destroy_t_target_t_artifact_t_and_all_other_artifacts_with_the_same_name_as_that_artifact-->[t_t_destroy, t_target_t_artifact, [t_and], [all_other_artifacts_with_the_same_name_as_that_artifact]].
t_t_destroy_t_six_target_creatures-->[t_t_destroy, t_six_target_creatures].
t_t_destroy_each_artifact_creature_and_enchantment_with_converted_mana_cost_X-->[t_t_destroy, each_artifact_creature_and_enchantment_with_converted_mana_cost_X].
t_t_destroy_each_artifact_with_converted_mana_cost_X_or_less-->[t_t_destroy, each_artifact_with_converted_mana_cost_X_or_less].
t_t_destroy_any_number_of_target_t_creatures-->[t_t_destroy, [any_number_of_target], [t_creatures]].
t_t_destroy_any_number_of_target_t_artifacts_and_or_t_enchantments-->[t_t_destroy, any_number_of_target_t_artifacts_and_or_t_enchantments].
t_t_destroy_all_artifacts-->[t_t_destroy, [all_artifacts]].
t_t_destroy_all_artifacts_t_creatures_t_and_t_lands-->[t_t_destroy, [all_artifacts], t_creatures_t_and_t_lands].
t_t_destroy_all_artifacts_t_creatures_t_and_t_enchantments-->[t_t_destroy, [all_artifacts], t_creatures_t_and_t_enchantments].
t_t_destroy_all_artifacts_t_and_t_enchantments-->[t_t_destroy, [all_artifacts], t_and_t_enchantments].
