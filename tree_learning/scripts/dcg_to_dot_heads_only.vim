" Remove empty lines
%g/^$/d
" Remove terminals synonymous to rule names
%s/^\(\w\+-->\)\[\w\+\],/\1/g
" Prepend "r_" to each rule
%s/^/r_/g
" Prepend "r_" to each nonterminal at end of rules
%s/\([>,]*\)\(\w\+\.\)$/\1r_\2/g
" Replace DCG arrows with dot directed edges
%s/-->/ -> /g
" Replace Prolog '.'s with dot ';'s
%s/\.$/;/g
" Remove square brackets around pre-terminals' rhs
%s/\[\|\]//g 
:normal ggO
:normal istrict digraph ability {
:normal o
:normal iordering="out"; 
:normal Go
:normal i}
" Write? 
":write! :p.dot
