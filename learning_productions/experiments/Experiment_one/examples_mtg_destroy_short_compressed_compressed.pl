:-module(examples_mtg_destroy_short_compressed_compressed,[example_string/1]).

example_string([t_t_destroy, [all_artifacts], t_and_t_enchantments]).
example_string([t_t_destroy, [all_artifacts], t_creatures_t_and_t_enchantments]).
example_string([t_t_destroy, [all_artifacts], t_creatures_t_and_t_lands]).
example_string([t_t_destroy, [all_artifacts]]).
example_string([t_t_destroy, any_number_of_target_t_artifacts_and_or_t_enchantments]).
example_string([t_t_destroy, [any_number_of_target], [t_creatures]]).
example_string([t_t_destroy, each_artifact_with_converted_mana_cost_X_or_less]).
example_string([t_t_destroy, each_artifact_creature_and_enchantment_with_converted_mana_cost_X]).
example_string([t_t_destroy, t_six_target_creatures]).
example_string([t_t_destroy, t_target_t_artifact, [t_and], [all_other_artifacts_with_the_same_name_as_that_artifact]]).
example_string([t_t_destroy, t_target_t_artifact, t_and_t_target_t_enchantment]).
example_string([t_t_destroy, t_target_t_artifact, t_creature_or_black_creature]).
example_string([t_t_destroy, t_target_white_creature]).
example_string([t_t_destroy, t_three_target_permanents]).
example_string([t_t_destroy, two_target_t_artifacts]).
example_string([t_t_destroy, 'two_target_nonblack_creatures_unless_either_one_is_a_color_the_other_isn\'t']).
example_string([t_t_destroy, 'X_target_t_artifacts']).
example_string([t_t_destroy, 'X_target_snow_lands']).
