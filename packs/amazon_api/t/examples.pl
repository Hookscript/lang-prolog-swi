:- use_module(library(amazon_api)).

build_agent(Agent) :-
    build_agent( Agent, _{ associate_tag: "TODO"
                         , access_key: "TODO"
                         , secret_key: "TODO"
                         }
               ).

:- dynamic item/2.


:- use_module(library(tap)).

'mario kart 64: fetch' :-
    build_agent(Agent),
    Args = _{ 'ResponseGroup': "ItemAttributes,OfferSummary" },
    item_lookup(Agent, 'B00000DMAX', Item, Args),
    assert(item(mario_kart_64, Item)).

'mario kart 64: title' :-
    item(mario_kart_64, Item),
    title(Item, Title),
    Title == 'Mario Kart 64'.

'mario kart 64: inventory' :-
    item(mario_kart_64, Item),
    offer_inventory(Item, used, Used),
    Used > 10,  % always this many available
    offer_inventory(Item, new, New),
    New > 1,    % always at least one available
    offer_inventory(Item, collectible, Collectible),
    Collectible > 1.

'mario kart 64: prices' :-
    item(mario_kart_64, Item),
    offer_low_price(Item, used, Used),
    Used > 15_00,  % $15 is cheap
    offer_low_price(Item, new, New),
    New > 100_00,  % $100 is cheap
    offer_low_price(Item, collectible, Collectible),
    Collectible > 15_00.  % random guess on a minimum
