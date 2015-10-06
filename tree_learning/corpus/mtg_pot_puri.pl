:-module(mtg_pot_puri, [example_string/1]).

example_string([destroy,target,creature]).
example_string([destroy,target,creature,if,it,is,black]).
example_string([exile,target,artifact]).
example_string([return,target,land,to,'it''s','owner''s',hand]).
example_string(['Lightning','Bolt',deals,3,damage,to,target,creature,or,player]).
example_string([target,creature,gains,flying,until,end,of,turn]).
example_string([enchanted,wall,can,attack,as,though,it,'didn''t',have,defender]).
example_string([basalt,monolith,'doesn''t',untap,during,your,untap,step]).
example_string(['{t}',:,add,'{3}',to,your,mana,pool]).
example_string(['{3}',:,untap,basalt,monolith]).
example_string([cast,blaze,of,glory,only,during,combat,before,blockers,are,declared]).
example_string([target,player,draws,x,cards]).
example_string([white,creatures,get,'+1',/,'+1']).


