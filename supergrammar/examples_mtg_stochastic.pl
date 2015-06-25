:-module(examples_mtg_stochastic, [example//2]).

example(destroy,T), [0.5] --> [destroy], target(T).
example(exile,T), [0.5] --> [exile], target(T).

target(T), [0.75] --> [target], permanent(T).
target(T), [0,25] --> [target], player(T).

permanent(prm(T)), [0.33] --> {T = tp(crt)}, type(T).
permanent(prm(T)), [0.33] --> {T = tp(art)}, type(T).
permanent(prm(T)), [0.33] --> {T = tp(lnd)}, type(T).

player(plr(T)), [0.5] --> {T = tp(plr)}, type(T).
player(plr(T)), [0.5] --> {T = tp(you)}, type(T).

type(tp(crt)), [0.2] --> [creature].
type(tp(art)), [0.2] --> [artifact].
type(tp(lnd)), [0.2] --> [land].
type(tp(plr)), [0.2] --> [player].
type(tp(you)), [0.2] --> [you].


