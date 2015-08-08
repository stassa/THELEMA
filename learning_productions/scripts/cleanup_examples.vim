" Script for post-processing tokenised examples files.

"Find all instances of escaped n't (as in don't, can't, isn't)
"Turn them into a single word; can,'\'',t --> 'can\'t' 
%s/'*\([a-z]\+\)'*,'\\'',t/'\1\\'t'/g 

"Do the same for 'creature's' etc: 
%s/'*\([a-zA-Z]\+\)'*,'\\'',s/'\1\\'s'/g 

"Remove reminder text:
%s/'('.*')'//g 

"Also remove '"''s left behind (when an ability grants a card an ability
" and it's stated in ""s like for eldrazi spanws:
" They',have,'"','Sacrifice',this,creature,:,'Add','{',1,'}',to,your,mana,pool,'.','"'): 
%s/'"',*//g

"Clean up full-stops at the end of sentences and put each sentence on its
"own line (better keep a backup with full abilities in one line). 
%s/,'\.',/\r/g  " remove '.'s in the middle of sentences
%s/,'\.'$//g    " Then remove them all

"Make mana symbols atomic: '{',1,'}' --> {1} etc:
%s/'{','*\(.\)'*,'}'/'{\1}'/g

" And the empty lines left behind.
g/^$/d

" Wrap each line in an example_string/1 clause.
%s/^/example_string([/g
%s/$/])./g

" Add a module declaration at the top of the file.
:normal ggO
:normal i:-module(RENAME_ME_YOU_ASS,[example_string/1]).
:normal o
