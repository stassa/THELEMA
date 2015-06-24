% Run this file with [run_demo].
% Edit configuration module to change example modules.

:-clear_productions. % First make sure there are no productions left from a previous run in a different examples module.
:-make. % Then make sure configuration module is up-to-date and pointing to the right examples module.

prolog:message(supergrammar(generate,1)) --> ['==================== Generating new production rules. ===================='].
prolog:message(supergrammar(generate,2)) --> ['========== Using language specification file defined in configuration.pl. ==========\n'].

:-print_message(informational, supergrammar(generate,1)).
:-print_message(informational, supergrammar(generate,2)).

% Set upper bounds of rule complexity and derivation length to be equal
% to the length of the longest example string, then start a new
% generate-and-test cycle.
:- configuration:examples_module(M)
  ,setof(Length,
	    Examples^(M:example_string(Examples)
	             ,length(Examples, Length))
	, Examples_lengths)
  ,reverse(Examples_lengths, [Longest_example|_Rest])
  ,writeln(examples_lengths:Examples_lengths)
  ,forall(generate(Longest_example, Longest_example,
		   50_000 % Inference limit- exit if reached before end of generate-and-test cycle.
		  , [production_bound(upper) % Make the number of tokens in a new production an upper bound; also accepts "exact"
		    ,derivation_bound(upper) % Make the number of tokens in a parse an upper bound; also accepts "exact"
		    ,ground(false) % Whether to ground terms in bodies of new productions; also accepts "true"
		   ]), true).

:-nl.
prolog:message(supergrammar(reporting)) --> ['==================== Reporting new productions. ====================\n'].

:-print_message(informational, supergrammar(reporting)).

:- portray_productions([print(new)]).

:-nl.
prolog:message(supergrammar(derivations)) --> ['==================== Derivations using the new productions. ====================\n'].

:-print_message(informational, supergrammar(derivations)).

% Print some production-derivation pairs for the user's learned
% appreciation.
:- setof(P-D
	,P^(supergrammar:derived_production(P, D))
	,Productions)
	,forall(member(P-D, Productions), writeln(P --> D)).

prolog:message(supergrammar(help_server, running, Port)) --> ['==================== PLDoc server running on port '], [Port],['. ====================\n'].
prolog:message(supergrammar(help_server, not_running)) --> ['==================== Starting help server. ====================\n'].

% Start the help server and direct the user to its current url.
:-use_module(library(http/thread_httpd)).

help_message:-
	thread_httpd:http_server_property(Port, goal(pldoc_http:http_dispatch))
	,nl
	,print_message(informational, supergrammar(help_server, running, Port))
	.
help_message:-
	nl
	,print_message(informational, supergrammar(help_server, not_running)).

:- help_message.

:-help(supergrammar).

