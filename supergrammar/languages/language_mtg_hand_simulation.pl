:-module(language_mtg_hand_simulation, [start//0
				       ,terminal//0
				       ,nonterminal//0
				       ]).
:-add_import_module(language_mtg_hand_simulation, language, start).

start_symbol --> [ability].
