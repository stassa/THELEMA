:-module(mtgjson_data,[write_ability_text_examples/3
		      ,ability_text_examples/3
		      ,card_data/3
		      ,card_characteristics/3
		      ,cards_data/2
		      ,select/4]).

:-use_module(library(http/json)).
:-use_module(library(error)).

%!	mtgjson_stream_alias// is det.
%
%	Alias for the stream from which mtgjson is read. Normallly this
%	would be a file.
mtgjson_stream_alias --> [mtgjson].

%!	mtgjson_identifier_separator// is det.
%
%	Separator for atomic card identifiers created from cardname
%	strings.
%       Eg, "Air Elemental --> air_elemental.
mtgjson_identifier_separator --> ['_'].

%!	mtg_data// is det.
%
%	Mtgjson elements to extract for various purposes.
mtg_data(card_characteristics) -->
	[name,manaCost,type,text,power,toughness,loyalty].

% Additional data from extras files.
mtg_data(extras) -->
	[name
	,number
	,multiverseid
	,rulings
	,foreignNames
	,printings
	,originalText
	,originalType
	,legalities
	,source].

% Additional characteristics from the main mtgjson data
mtg_data(main) --> [name
		   ,manaCost
		   ,cmc
		   ,colors
		   ,type
		   ,supertypes
		   ,types
		   ,subtypes
		   ,rarity
		   ,text
		   ,number
		   ,power
		   ,toughness
		   ,loyalty
		   ,multiverseid
		   ,watermark].


/*
Examples:
mtgjson_card_dicts('LEA.json', Cards, minimum), member(C,Cards), C.name = "Lightning Bolt", !, Cost=C.manaCost.
*/
/*
Plan:
1. First, make these horrid loops build up a dict rather than a list, so
I can refer to its elements by name etc. - DONE
2. Make a predicate to get one card by name, then exploit it to get all
the minimum and extras information. - TODO
*/

% TODO: Update the comments according to the latest changes.

%!	write_ability_text_examples(+Stream_in, +Stream_out) is det.
%
%	Read ability text from the cards in Stream_in and write them to
%	Stream_out, each on a separate line.
%	Stream_in should be a json file with card information (but not
%	set information).
%	Each ability is written in a separate line, as they are found on
%	the printed cards (or in any case, Oracle text). No other
%	information is included- no card name, mana cost etc, so this is
%	only useful to get some statistics on the corpus.
%	On the other hand, Oracle text makes no distinction between
%	ability text proper and reminder text (which is presumably in
%	plain natural language) so this is included in the examples,
%	although it can be identified because it's always in
%	parentheses.
%	On the other tentacle, basic lands actually have their ability
%	(to add mana to your pool) as reminder text, ie in parentheses.
%	So might need to refine this a bit further.
%	Oof. See notes about this: [reminder_text]
write_ability_text_examples(Stream_in, Stream_out, Options):-
	ability_text_examples(Stream_in, Examples, Options)
	,open(Stream_out,write,Alias,[alias(mtgjson_in),encoding(utf8)])
	,write_ability_text_examples_list(mtgjson_in, Examples)
	,close(Alias)
	,!. %Green

write_ability_text_examples_list(_,[]).
write_ability_text_examples_list(Stream, [Example|Examples]):-
	% HACK: temporary measure to deal with vanilla creatures
	(   var(Example)
	->  write(Stream, vanil)
	;   write(Stream, Example)
	)
	,write(Stream,"\n")
	,write_ability_text_examples_list(Stream,Examples).


%!	ability_text_examples(+Stream,-Examples,+Options) is det.
%
%	Raw dump of ability text read from Stream, meant to be used for
%	training data for some machine learning algorithm.
%
%	Options:
%	  unsets(Boolean): whether to print examples from "Un"
%	  silver-bordered sets, Unglued and Unhinged.
%
ability_text_examples(Stream, Examples, [unsets(false)]):-
	(   Stream \==  'AllSets.json'
	->  domain_error('AllSets.json', Stream)
	;   true
	)
	,findall(Ability_text
	       ,(card_characteristics(Stream, Set:_, Characteristics)
		,Set \== 'UGL', Set \== 'UNH'
		,Characteristics >:< _{text:Ability_text})
	       ,Examples).

ability_text_examples(Stream, Examples, [unsets(true)]):-
	(   Stream == 'AllSets.json'
	->  domain_error('AllCards.json or specific set json file name', Stream)
	;   true
	)
	,findall(Ability_text
	       ,(card_characteristics(Stream, _, Characteristics)
		,Characteristics >:< _{text:Ability_text})
	       ,Examples).


%!	card_characteristics(+Stream,?Cardname,?Characteristics) is  nondet.
%
%	Stream is a stream of M:tG data in Json format. Card_id is an
%	atomic card name. Characteristics is bound to those
%	characteristics of the card that are most important when playing
%	the game: name, mana cost, type line, ability text
%	power/toughness and loyalt (where relevant).
%
%	Analytical information such as colour, cmc, type line broken
%	down in supertype, type and subtype etc is not provided (TODO:
%	make a predicate that returns specified elements).
%
%	TODO two: separate ability text from reminder text- the latter
%	could be useful as metadata/ explanatory data.
%
%	TODO three: Maybe make this use atomic ids from mtg_identifier
%	again?
card_characteristics('AllSets.json', Set:Cardname, Characteristics):-
	card_data('AllSets.json', Set:Cardname, Card_data)
	,phrase(mtg_data(card_characteristics), Keys, [])
	,(
	     nonvar(Cardname)
	     -> mtg_identifier(Cardname, Identifier)
	     ; Identifier = Card_data.name
	 )
	,select(Card_data,Keys,Identifier,Characteristics).

card_characteristics(Stream, Card_id, Characteristics):-
	% To stop backtracking into this and forever
	% after failing the last try above.
	Stream \== 'AllSets.json'
	,card_data(Stream, Card_id, Card_data)
	,phrase(mtg_data(card_characteristics), Keys, [])
	,(
	     nonvar(Card_id)
	     -> mtg_identifier(Card_id, Identifier)
	     ; Identifier = Card_data.name
	 )
	,select(Card_data,Keys,Identifier,Characteristics).
/*
Example use, per scope:
card_characteristics('AllCards.json','Lightning Bolt', Chars).
card_characteristics('AllSets.json', Set:'Lightning Bolt', Chars).
card_characteristics('LEA.json', 'Lightning Bolt', Chars).

Remember that scope AllSets.json needs Set:Card Id
*/


%!	card_data(+Json,?Card_id,?Card_data) is nondet.
%
%	Read card data from the stream Json as a dict and bind to
%	Card_data.
%
%	Json must be a stream of mtgjson data and so must conform to the
%	json structure for that project. Currently, only text files are
%	supported and they must have one of the following names:
%
%	AllCards.json: raw dump of card data from all sets
%	AllSets.json: all sets data
%	<SetCode>.json, where SetCode is an mtg set code: [A-Z]\{3,3}
%
%	TODO: no extra information, currently- add that.
%
%	Card_id is an identifier for the card. It can be one of two
%	things:
%
%	The name of a card given as an atom or a Swi-Prolog string, eg
%	'Air Elemental', "Air Elemental" etc. This option should be used
%	with scope(all_cards) and scope(one_set) (see below).
%
%	A compound Set_code:Card_name, where Set_code is the name of the
%	set where the card is to be found and Card_name the card name as
%	above. Either or both of these components can be left unbound
%	to enumerate answers. This option only make sense with
%	scope(all_sets) and will fail with errors otherwise anyway.
%
card_data(Stream, Cardname, Card_data):-
	phrase(mtgjson_stream_alias, [Alias])
	,open(Stream,read,_,[alias(Alias),encoding(utf8)])
	,json_read_dict(Alias,Cards)
	,close(Alias)
	,(
	     Stream == 'AllCards.json'
	     -> read_card_data(Cards, Cardname, Card_data, scope(all_cards))
	     ; Stream == 'AllSets.json'
	     -> read_card_data(Cards, Cardname, Card_data, scope(all_sets))
	     ; read_card_data(Cards, Cardname, Card_data, scope(one_set))
	 ).


%!	read_card_data(+Cards,?Card_Id,-Card_data,Options) is nondet.
%
%	Business end of card_data. Card_id is passed in from card_data.
%	Cards is either a dict-of-dicts with card information read from
%	an mtgjson file, holding card or set data depending on scope
%	(see below).
%
%	Options is a list of options, currently:
%
%	scope(Scope), determines to the structure of the json to read
%	from and corresponds to the mtgjson names of card data files.
%	It's one of: all_cards, one_set, all_sets.
%
%	With scope(all_cards) data is read from 'AllCards.json', with
%	scope(one_set) data is read from a specific set file, eg
%	LEA.json, TMP.json etc (given as a stream name) and with
%	scope(all_sets) data is read from AllSets.json.
%
read_card_data(Cards, Cardname, Card_data, scope(all_cards)):-
	(   nonvar(Cardname)
	->  atom_string(Atomic, Cardname) % Ensure name is passed as atom
	;   true
	)
	,Card_data = Cards.Atomic.
% card_data('AllCards.json', 'Lightning Bolt', Card_data, scope(all_cards)).

%  card_data('LEA.json',"Lightning Bolt",Card_data,scope(one_set)).
read_card_data(Set, Cardname, Card_data, scope(one_set)):-
	(   nonvar(Cardname)
	->  atom_string(Cardname, String) % Ensure name is passed as a string
	;   true
	)
	,Cards = Set.cards
	,member(Card,Cards)
	,Card.name = String
	,Card_data = Card.

% card_data('AllSets.json','LEB':"Lightning Bolt",Card_data,scope(all_sets)).
% Note how this clause doesn't check whether the set code is atomic or
% not.
read_card_data(Sets, Set:Cardname, Card_data, scope(all_sets)):-
	(   nonvar(Cardname)
	->  atom_string(Cardname, String)
	;   true
	)
	,Cards = Sets.Set.cards
	,member(Card,Cards)
	,Card.name = String
	,Card_data = Card.


%!	cards_data(+Mtgjson,-Cards_data) is det.
%
%	Read card data from Mtgson and bind to Cards_data as a
%	dict-of-dicts where each sub-dict is one card.
cards_data(Stream, Cards_data):-
	phrase(mtgjson_stream_alias, [Alias])
	,open(Stream,read,_,[alias(Alias),encoding(utf8)])
	,json_read_dict(Alias,Data)
	,close(Alias)
	,(
	     Stream == 'AllCards.json'
	     -> read_cards_data(Data, Cards_data, scope(all_cards))
	     ; Stream == 'AllSets.json'
	     -> read_cards_data(Data, Cards_data, scope(all_sets))
	     ; read_cards_data(Data, Cards_data, scope(one_set))
	 ).


%!	read_cards_data(+Data,-Cards,+Options) is nondet.
%
%	Business end of cards_data/2. Options are like read_card_data/3
read_cards_data(Cards_data, Cards_data, scope(all_cards)).

read_cards_data(Data, Cards_data, scope(all_sets)):-
	findall(Cards
		,select_dict(_{cards:Cards},Data._Set,_)
		,Cards_list)
	,flatten(Cards_list, Flat)
	,mtgjson_dicts(Flat, name, cards{}, Cards_data).

read_cards_data(Data, Cards_data, scope(one_set)):-
	select_dict(_{cards:Cards},Data,_)
	,mtgjson_dicts(Cards, name, cards{}, Cards_data).


%!	mtgjson_dicts(+Cards,+Key,+Temp,-Acc) is nondet.
%
%	Business end of mtgjson_dict, where scope(all_sets_card_data).
%	Builds up a dict of dicts with all the card data in all sets
%	from AllSets.json. Key is the name of the key used to identify
%	a card, normally name.
mtgjson_dicts([],_,Dict,Dict).
mtgjson_dicts([C|Cs],Key,Temp,Acc):-
	mtg_identifier(C.Key, Identifier)
	,Dict = Temp.put(Identifier, C)
	,mtgjson_dicts(Cs,Key,Dict,Acc).


%!	mtg_cardname(+Identifier, -Card_name) is det.
%
%	Convert an atomic identifier (downcased, spaces converted to
%	'_') to a propa card name (camelcased, spaces, atomic).
mtg_cardname(Identifier, Card_name):-
	phrase(mtgjson_identifier_separator, [Separator])
	,atomic_list_concat(L, Separator, Identifier)
	,mtg_cardname(L, [], Name_components)
	,atomic_list_concat(Name_components, ' ', Card_name).


%!	mtg_cardname(+Identifier,+Temp,-Acc) is nondet.
%
%	Business end of mtg_cardname/2
mtg_cardname([], Emandrac, Cardname):-
	reverse(Emandrac, Cardname).
mtg_cardname([A|As], Temp, Acc):-
	atom_chars(A, [C|Chars])
	,upcase_atom(C,Up)
	,atomic_list_concat([Up|Chars], '',  Concat)
	,mtg_cardname(As, [Concat|Temp], Acc).


%!	mtg_identifier(+String, -Atomic) is det.
%
%	Convert between a string and an atomic identifier.
mtg_identifier(Card_name, Identifier):-
	phrase(mtgjson_identifier_separator, [Separator])
	,string_lower(Card_name, L)
	,split_string(L, " ", "", Sub)
	,atomic_list_concat(Sub, Separator, Identifier).
/*
Hmeh. This may not be that bad:

[debug] 123 ?- string_lower("Air Elemental", L), split_string(L, " ", "", Sub), atomic_list_concat(Sub,'_',Conc).
L = "air elemental",
Sub = ["air", "elemental"],
Conc = air_elemental.

[debug] 124 ?- time((string_lower("Air Elemental", L), split_string(L, " ", "", Sub), atomic_list_concat(Sub,'_',Conc))).
% 6 inferences, 0.000 CPU in 0.000 seconds (?% CPU, Infinite Lips)
L = "air elemental",
Sub = ["air", "elemental"],
Conc = air_elemental.

	*/


%!	select(+Dict,+Keys,?Tag,-New_dict) is det.
%
%	New_dict is a dict with an optional Tag, keys the members of
%	the list Keys that appear in Dict and values the values of the
%	respective Keys in Dict.
%
%	Any members of Keys that are not keys in Dict will be dropped
%	silently.
%
%	Fails silently if Dict or Keys are unbound.
%
%	TODO: this is a prime candidate for moving to a library module.
select(Dict, Keys, Tag, Dict_values):-
	nonvar(Dict)
	,nonvar(Keys)
	,findall(Key-Value
		 % Dict.get(Key) fails without error if Key is not there.
	       ,(member(Key,Keys),Value = Dict.get(Key))
	       ,KVPs)
	,kvp_dicts(KVPs, Tag{}, Dict_values).


%!	kvp_dicts(+Key_value_pairs,+Temp,-Acc) is det.
%
%	Business end of select/4. Builds up a new dict from the
%	given Key_value_pairs.
kvp_dicts([],Dict,Dict).
kvp_dicts([Key-Value|KVPs],Temp,Acc):-
	Dict = Temp.put(Key, Value)
	,kvp_dicts(KVPs,Dict,Acc).

