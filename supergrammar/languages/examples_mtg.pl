:-module(examples_mtg, [example//0
		       ,example_string/1]).


example --> {example_string(S)}, S.

%!	examples_string(?Example) is nondet.
%
%       A set of example strings that we know belong to the language.
example_string(['Lightning Bolt',deals,3,damage,to,target,creature,or,player]).
example_string(['Fireball',deals,'X',damage,to,target,creature,or,player]).
example_string([destroy,target,creature]).
example_string([destroy,target,artifact]).
example_string([exile,target,creature]).
example_string([return,target,creature,to,its,'owner''s',hand]).


















