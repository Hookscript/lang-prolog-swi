:- use_module(library(uri_qq)).

:- use_module(library(tap)).

% I don't want to run http_get/3 from the tests, so this is
% only an approximation of what really happens in the Synopsis.
% It should be close enough to catch any mistakes I might make.
synopsis :-
    Path = search,
    Params = [q='prolog is awesome'],
    format(atom(Uri), '~w', [{|uri||google.com/$Path?$Params|}]),
    Uri = 'http://google.com/search?q=prolog%20is%20awesome'.
