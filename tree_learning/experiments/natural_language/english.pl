:-module(english, [start//0
		  ,terminal//0
		  ,nonterminal//0
		  ]).
:-add_import_module(english, language, start).

start_symbol --> [sentence].

