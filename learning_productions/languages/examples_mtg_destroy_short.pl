:-module(examples_mtg_destroy_short, [example_string/1]).

example_string([destroy,all,artifacts,and,enchantments]).
example_string([destroy,all,artifacts,creatures,and,enchantments]).
example_string([destroy,all,artifacts,creatures,and,lands]).
example_string([destroy,all,artifacts]).
example_string([destroy,any,number,of,target,artifacts,and/or,enchantments]).
example_string([destroy,any,number,of,target,creatures]).
example_string([destroy,each,artifact,with,converted,mana,cost,'X',or,less]).
example_string([destroy,each,artifact,creature,and,enchantment,with,converted,mana,cost,'X']).
example_string([destroy,six,target,creatures]).
example_string([destroy,target,artifact,and,all,other,artifacts,with,the,same,name,as,that,artifact]).
example_string([destroy,target,artifact,and,target,enchantment]).
example_string([destroy,target,artifact,creature,or,black,creature]).
example_string([destroy,target,white,creature]).
example_string([destroy,three,target,permanents]).
example_string([destroy,two,target,artifacts]).
example_string([destroy,two,target,nonblack,creatures,unless,either,one,is,a,color,the,other,'isn''t']).
example_string([destroy,'X',target,artifacts]).
example_string([destroy,'X',target,snow,lands]).

