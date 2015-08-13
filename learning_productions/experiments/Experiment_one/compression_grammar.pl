:-module(compression_grammar, [compression_grammar//1]).

compression_grammar([])-->[].
compression_grammar([A|As])-->nonterminal(A), !, compression_grammar(As).
compression_grammar([A|As])-->terminal(A), !, compression_grammar(As).

nonterminal(t_t_destroy_X_target_snow_lands)-->[t_t_destroy, 'X_target_snow_lands'].
nonterminal(t_t_destroy_X_target_t_artifacts)-->[t_t_destroy, 'X_target_t_artifacts'].
nonterminal('t_t_destroy_two_target_nonblack_creatures_unless_either_one_is_a_color_the_other_isn\'t')-->[t_t_destroy, 'two_target_nonblack_creatures_unless_either_one_is_a_color_the_other_isn\'t'].
nonterminal(t_t_destroy_two_target_t_artifacts)-->[t_t_destroy, two_target_t_artifacts].
nonterminal(t_t_destroy_t_three_target_permanents)-->[t_t_destroy, t_three_target_permanents].
nonterminal(t_t_destroy_t_target_white_creature)-->[t_t_destroy, t_target_white_creature].
nonterminal(t_t_destroy_t_target_t_artifact_t_creature_or_black_creature)-->[t_t_destroy, t_target_t_artifact, t_creature_or_black_creature].
nonterminal(t_t_destroy_t_target_t_artifact_t_and_t_target_t_enchantment)-->[t_t_destroy, t_target_t_artifact, t_and_t_target_t_enchantment].
nonterminal(t_t_destroy_t_target_t_artifact_t_and_all_other_artifacts_with_the_same_name_as_that_artifact)-->[t_t_destroy, t_target_t_artifact, [t_and], [all_other_artifacts_with_the_same_name_as_that_artifact]].
nonterminal(t_t_destroy_t_six_target_creatures)-->[t_t_destroy, t_six_target_creatures].
nonterminal(t_t_destroy_each_artifact_creature_and_enchantment_with_converted_mana_cost_X)-->[t_t_destroy, each_artifact_creature_and_enchantment_with_converted_mana_cost_X].
nonterminal(t_t_destroy_each_artifact_with_converted_mana_cost_X_or_less)-->[t_t_destroy, each_artifact_with_converted_mana_cost_X_or_less].
nonterminal(t_t_destroy_any_number_of_target_t_creatures)-->[t_t_destroy, [any_number_of_target], [t_creatures]].
nonterminal(t_t_destroy_any_number_of_target_t_artifacts_and_or_t_enchantments)-->[t_t_destroy, any_number_of_target_t_artifacts_and_or_t_enchantments].
nonterminal(t_t_destroy_all_artifacts)-->[t_t_destroy, [all_artifacts]].
nonterminal(t_t_destroy_all_artifacts_t_creatures_t_and_t_lands)-->[t_t_destroy, [all_artifacts], t_creatures_t_and_t_lands].
nonterminal(t_t_destroy_all_artifacts_t_creatures_t_and_t_enchantments)-->[t_t_destroy, [all_artifacts], t_creatures_t_and_t_enchantments].
nonterminal(t_t_destroy_all_artifacts_t_and_t_enchantments)-->[t_t_destroy, [all_artifacts], t_and_t_enchantments].

terminal(T)-->{phrase(terminal, T)}, T.

terminal-->['X_target_snow_lands'].
terminal-->['X_target_t_artifacts'].
terminal-->[any_number_of_target_t_artifacts_and_or_t_enchantments].
terminal-->[each_artifact_creature_and_enchantment_with_converted_mana_cost_X].
terminal-->[each_artifact_with_converted_mana_cost_X_or_less].
terminal-->[t_and_t_enchantments].
terminal-->[t_and_t_target_t_enchantment].
terminal-->[t_creature_or_black_creature].
terminal-->[t_creatures_t_and_t_enchantments].
terminal-->[t_creatures_t_and_t_lands].
terminal-->[t_six_target_creatures].
terminal-->[t_t_destroy].
terminal-->[t_target_t_artifact].
terminal-->[t_target_white_creature].
terminal-->[t_three_target_permanents].
terminal-->['two_target_nonblack_creatures_unless_either_one_is_a_color_the_other_isn\'t'].
terminal-->[two_target_t_artifacts].
terminal-->[[all_artifacts]].
terminal-->[[all_other_artifacts_with_the_same_name_as_that_artifact]].
terminal-->[[any_number_of_target]].
terminal-->[[t_and]].
terminal-->[[t_creatures]].
