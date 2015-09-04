:-module(examples_mtg_hand_simulation, [example_string/1]).


%!	examples_string(?Example) is nondet.
%
%       A set of example strings that we know belong to the language.
example_string([destroy,target,creature]).
example_string([destroy,target,artifact]).
example_string([exile,target,creature]).
%example_string([destroy,target,creature,if,it,is,black]).








