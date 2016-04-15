# TheLeMa
A THEory LEarning MAchine for grammar induction. 

TheLeMa is an inductive logic programming algorithm and a program based on that
algorithm. The algorithm learns a graph structure from sequential data, for
instance a grammar from examples of sentences in a tagret language. 

For the time being TheLeMa is taking its first baby steps in its very own blocks
world: the Controlled Natural Language at the heart of the Collectible Card Game
Magic: the Gathering known as "Ability Text" (there is no affiliation between
this project and the publishers of that game). 

Here is a diagram of a grammar TheLeMa learned from a set of 177 examples of
Ability Text, all insances of the "destroy" ability: 

![Alt text](https://cdn.rawgit.com/stassa/THELEMA/images/readme_image_files/all_destroy_lexicalised.svg?raw=true "Lexicalised Restricted-Greibach Normal Form (177 examples)")


Using TheLeMa
=============

TheLeMa is written in Prolog. You will need to have Swi-Prolog 7.0 or higher
installed on your system in order to run it.

Enough with the technical details. Here's how to run this thing
---------------------------------------------------------------

Start TheLeMa by loading the file: 

```
tree_learning/load_tree_learning.pl
```

On a windows machine you can double-click the file and it will open the Prolog
console and the Swi-Prolog IDE. On Linux: 

1. change directory to tree\_learning
2. start Swi-Prolog,  and 
3. enter "[load\_tree\_learning]."

On Linux that will *probably* not bring up the IDE. But you never know. 

The shiny happy path through the application
============================================

To begin training TheLeMa you need to edit the configuration file: 

```
tree_learning/configuration.pl
```

You should read the comments in that file. They will probably make sense after a
while. To begin with, make sure you have the following options set: 

```
examples_file_name(examples_mtg_hand_simulation).
language_file_name(language_mtg_hand_simulation).
lexicalisation_strategy(none).
output_type(dcg).
production_augmentation(greibach).
production_composition(synonym).
rename_built_ins(n_).
```

If you need to change a setting, remember to enter: 

``` 
make, [configuration]. 
```

At the Prolog top-level, or the changes won't take. The '[configuration]' bit is
needed to ensure a newly specified corpus is actually loaded.

With the above settings, TheLeMa will learn a non-lexicalised Context-Free
Grammar from the "examples\_mtg\_hand\_simulation" mini-corpus (as its name
implies this corpus was used for hand-simulations which explains why it's so
tiny).

Start training by entering this query at the Prolog top-level: 

```
print_grammar. 
```

TheLeMa will place a grammar file in tree\_learning/output/ named after the
configured examples and language file so that you can easily identify it.  

Parsing and generating Ability Text with Definite Clause Grammars
=================================================================

TheLeMa's "native" grammar format is Prolog's Definite Clause Grammars notation.
DCGs have a direct translation to Horn Clauses so in most Prologs and indeed in
Swi-Prolog, DCG rules will automatically compile to ordinary Prolog predicates
when you load their source file. This might sound like a load of jargon but the
upshot of this is that the grammar that TheLeMa learns is really a program in
Prolog. In fact, it's a parser and also generator for strings in the language
described by the grammar. 

You can verify this by doing the following: 

1. Load the output grammar file from the Shiny Happy Path section into a new
Prolog instance (don't use the old one or you'll get told off)
2. Enter this query at the Prolog top-level: 
   
```
forall(phrase(ability, P), writeln(P)). 
```

If you followed the Shiny Happy Path to the letter, the above query should give
you a set of nice little derivations reproducing the examples in the training
corpus and even generalising a little. 

If you *didn't* follow the Shiny Happy Path then woe is you. Larger datasets
tend to produce grammars with recursive paths running though them. If you try to
generate new text from such a grammar Prolog will lock. If... *when* that happens
you can abort execution with "Ctrl + C" and then "a", or by killing the Prolog
instance. 

You can also use the induced grammar as a recogniser, to accept or reject
strings. 

Try this; at the Prolog top-level, on the same instance where you loaded the
output of the Shiny Happy Path, enter the following queries: 

```
phrase(ability, [destroy,target,artifact]).
phrase(ability, [destroy,target,X]).
phrase(ability, [destroy,target,stassa]). 
```

The first query should say "true" and then wait for input. Press "enter" to end
the search for more answers (if you press ";" the query exits with "false"
because it can't find more results that match its pattern). 

The second query should bind "X" to "artifact" and wait for input. Press ";" or
the space bar for more results and "X" will bind to "creature". Press enter to
end the search or ";" to see the "false". 

The third query lived in a house of stone. There is no such derivation possible
in the grammar you loaded, therefore Prolog will immediately say "false" and
end its search for answers. In any case, you should never destroy stassas, they
don't like that. They do like chocolate, however. Do you have chocolate? If you
have chocolate then you can proceed to the next section.

Grammar formalisms and output formats
=====================================

TheLeMa is fast and unsupervised, but for every type of grammar you want it to
learn you need to create a new set of predicates. That can hurt a little, but
it's possibly possible to automate (by training a classifier to do it). 

For now TheLeMa has rules for a few types of normal form grammars. The simplest
of these is a determinstic Context-Free Grammar in a restricted Greibach Normal
Form, where all rules are of the form: 

``` 
A → aA 
S → A
```

In other words, a single terminal followed by a single nonterminal. Note that
the single terminal is always the synonym of the production's left-hand side;
essentially the solitary terminal is a label annotating the node on the
retrieved graph that the production represents. 

An "extended" form of GNF and also lexicalised versions of both forms are also
possible. There is also an option to learn a Chomsky Normal Form grammars but,
currently, training with large corpora is infeasible unless the Prolog stack is
increased. More efficient CNF and other grammar formalisms will follow in the
future. 

Visualising grammar productions
-------------------------------

TheLeMa is used for grammar induction but the first step in learning a grammar
is retrieving the structure of a graph from example sequences. It's easy enough
to visualise a context-free grammar as a graph with arcs mapping the relations
between nonterminals. 

To see this in action, set the following option in configuration.pl:

``` 
output_type(lean_dot). 
```

Then train TheLeMa with "print\_grammar" as above. TheLeMa will then print out
its induced grammar in the dot-language format used for visualisation (the file
is still placed in tree\_learning/output). You can feed that file directly into
a visualisation package such as Graphviz to generate a diagram of the induced
grammar. This is how this cute little image was created: 

![Alt text](/../images/readme_image_files/destroy_short_lexicalised_rgnf.png?raw=true "Lexicalised Restricted-Greibach Normal Form (18 examples)")

With a bit of elbow grease you can even visualise a grammar as a network by
exporting it into a package like Gephi or Cytoscape. See the at the start of
this readme file for example; it's the visualisation of a grammar learned from
177 examples and rendered as a network diagram in Gephi.

TheLeMa can also print out its grammar in the BNF and EBNF formats used in
parser generators such as Yacc and Bison. Neither of these formats is
particularly strictly implemented at this point, but you can always play around
with them a bit and see if you can get them to work with your favourite compiler
compiler. 
