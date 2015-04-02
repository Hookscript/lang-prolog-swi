:- module(amazon_api, [ build_agent/2
                      , item_lookup/3
                      , item_lookup/4
                      , request/5

                      , offer_inventory/3
                      , offer_low_price/3
                      , title/2
                      ]).

:- use_module(library(base64), [base64/2]).
:- use_module(library(error), [existence_error/2]).
:- use_module(library(func)).
:- use_module(library(http/http_open), [http_open/3]).
:- use_module(library(sha), [hmac_sha/4]).
:- use_module(library(uri), [uri_encoded/3]).
:- use_module(library(uri_qq)).
:- use_module(library(xpath)).


%% build_agent(-Agent, +Args:dict)
%
%  Build an Agent which is used for making Amazon API requests. Args
%  should contain at least the following keys (obtained from Amazon):
%
%    * `associate_tag` - string
%    * `access_key` - string
%    * `secret_key` - string
%
%  It may include an optional `host` key which defaults
%  to "webservices.amazon.com".
build_agent(Agent,Args) :-
    must_have(Args, associate_tag, string, Tag),
    must_have(Args, access_key, string, AccessKey),
    must_have(Args, secret_key, string, SecretKey),
    once( get_dict(host, Args, Host)
        ; Host = "webservices.amazon.com"
        ),
    Agent = agent(Host,Tag,AccessKey,SecretKey).

must_have(Args,Key,Type,Value) :-
    once( get_dict(Key, Args, Value)
        ; existence_error(Type, Key)
        ).


% private accessors for agent components
agent_host(agent(Host,_,_,_),Host).
agent_tag(agent(_,Tag,_,_),Tag).
agent_key(agent(_,_,Key,_),Key).
agent_secret(agent(_,_,_,Secret),Secret).


%% item_lookup(+Agent, +Asin:string, -Item, +Args:dict)
%
%  Perform an "ItemLookup" call using Amazon's Product Advertising API.
%  Additional API arguments are passed via Args. A common use case is to
%  specify response groups:
%
%  ==
%  item_lookup(Agent,'B00004TN1Z',Item, _{'ResponseGroup':"Offers"}).
%  ==
item_lookup(Agent, Asin, Item, Args0) :-
    Args = Args0.put(_{ 'ItemId' : Asin
                      , 'IdType' : "ASIN"
                      }),
    request(Agent, get, "ItemLookup", Item, Args).

%% item_lookup(+Agent, +Asin:string, -Item)
%
%  Like item_lookup/4 with no arguments.
item_lookup(Agent,Asin,Item) :-
    item_lookup(Agent,Asin,Item,_{}).


%% request(+Agent,+Method:atom,+Operation:string,-Result,+Args:dict)
%
%  Low level predicate for performing Amazon API requests. Executes
%  Operation using HTTP Method.  API arguments are provided in
%  Args. On success, a parsed representation of the response XML
%  is bound to Result. See library(sgml) for the format.
%
%  library(xpath) is helpful for extracting details from Result if
%  there's not already a helper predicate to handle it.
request(Agent, Method, Operation, Result, Args0) :-
    agent_tag(Agent, Tag),
    agent_key(Agent, Key),
    Args1 = Args0.put([ 'Service' = "AWSECommerceService"
                      , 'Version' = "2011-08-01"
                      , 'Operation' = Operation
                      , 'AWSAccessKeyId' = Key
                      , 'AssociateTag' = Tag
                      ]),
    sign_request(Agent, Method, Args1, Args),

    agent_host(Agent, Host),
    Url = {|uri||http://$Host/onca/xml?$Args|},
    url_dom(Url, Result).


% add timestamp and signature to Args0 giving Args
sign_request(Agent, Method, Args0, Args) :-
    % what string should we sign?
    Args1 = Args0.put('Timestamp', timestamp(~)),
    agent_host(Agent, Host),
    signable_string(Method, Host, Args1, SignMe),

    % perform the signature
    agent_secret(Agent, Secret),
    hmac_sha(Secret, SignMe, HmacBytes, [algorithm(sha256)]),
    base64(string_codes(~,HmacBytes), Signature),
    Args = Args1.put('Signature', Signature).


% string to be signed to generate request signature
signable_string(Method, Host, Args, String) :-
    encode_pairs(Args, EncodedPairs),
    String = "~s~n~s~n~s~n~s" $ [ upcase_atom $ Method
                                , Host
                                , "/onca/xml"
                                , EncodedPairs
                                ].

% encode request parameters into Amazon's canonical signature format
encode_pairs(Dict, Atom) :-
    dict_pairs(Dict, _, Pairs),
    keysort(Pairs, Sorted),
    maplist(encode_value, Sorted, Encoded),
    atomic_list_concat(Encoded, "&", Atom).


encode_value(K-V0,String) :-
    uri_encode(V0, V),
    String = "~s=~s" $ [K,V].


% encode URI values as Amazon expects.
% uri_encoded/3 doesn't encode comma, :, / or ? characters.
uri_encode(Value, Encoded) :-
    uri_encoded(query_value, Value, E0),
    atom_codes(E0, E1),
    once(phrase(enc, E1, E2)),
    atom_codes(Encoded, E2).

enc, "%2C" --> ",", enc.
enc, "%2F" --> "/", enc.
enc, "%3A" --> ":", enc.
enc, "%3F" --> "?", enc.
enc, [C] --> [C], enc.
enc --> { true }.


%% timestamp(-Timestamp:string) is det.
%
%  Generate a timestamp compatible with Amazon's API.
timestamp(T) :-
    get_time(Now),
    stamp_date_time(Now, DT, 'UTC'),
    format_time(string(T), "%FT%T.000Z", DT).


%% url_dom(+Url:atom, -Dom)
%
%  True if Dom is the parsed XML content at Url.
url_dom(Url, Dom) :-
    setup_call_cleanup( http_open(Url, In, [timeout(10)])
                      , url_dom_(Dom, In)
                      , close(In)
                      ).

url_dom_(Dom, Stream) :-
    load_structure( Stream
                  , [Dom|_]
                  , [ dialect(xml)
                    , max_errors(-1)
                    , syntax_errors(quiet)
                    ]
                  ).


%% offer_inventory(+Item,?Condition:atom,-Inventory:integer)
%
%  Find how many offers in Condition are available. Item is a value
%  produced by item_lookup/3, which must include the response group
%  "OfferSummary" (directly or indirectly).
%
%  Condition should be one of `used`, `new` or `collectible`. Leaving
%  Condition unbound iterates all conditions on backtracking.
offer_inventory(Item, Condition, Inventory) :-
    inventory_node(Condition, InventoryNode),
    once(xpath(Item, //'OfferSummary'/InventoryNode, Total)),
    Total = element(_,_,[InventoryAtom]),
    atom_number(InventoryAtom, Inventory).

inventory_node(used,        'TotalUsed').
inventory_node(new,         'TotalNew').
inventory_node(collectible, 'TotalCollectible').


%% offer_low_price(+Item, ?Condition, -Pennies:integer)
%
%  Find the lowest price for an offer in Condition. The price is
%  represented as an integer number of pennies, when using US Dollars.
%  An equivalent unit is used for other currencies. Item is a value
%  produced by item_lookup/3, which must include the response group
%  "OfferSummary" (directly or indirectly).
%
%  Condition should be one of `used`, `new` or `collectible`. Leaving
%  Condition unbound iterates all conditions on backtracking.
offer_low_price(Item, Condition, Pennies) :-
    price_node(Condition, PriceNode),
    once(xpath(Item, //'OfferSummary'/PriceNode/'Amount'(text), PriceAtom)),
    atom_number(PriceAtom, Pennies).

price_node(used,        'LowestUsedPrice').
price_node(new,         'LowestNewPrice').
price_node(collectible, 'LowestCollectiblePrice').


%% title(+Item, -Title:atom) is det.
%
%  An item's title. Item is a value from item_lookup/3 and requires the
%  "ItemAttributes" response group.
title(Item, Title) :-
    once(xpath(Item, //'ItemAttributes'/'Title'(text), Title)).
