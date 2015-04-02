:- use_module(library(uri_qq)).

% define helper predicates here

:- use_module(library(tap)).

'fully static URI' :-
    U = {|uri||http://example.com/path?query=yes|},
    U = 'http://example.com/path?query=yes'.

'suffix URI reference' :-
    U = {|uri||example.com/path|},
    U = 'http://example.com/path'.

'relative URI reference' :-
    Base = 'https://example.org/path/',
    Path = 'to/resource',
    PathTerm = to/resource,
    U = {|uri(Base)||to/resource|},
    U = {|uri(Base)||/path/to/resource|},
    U = {|uri(Base)||$Path|},
    U = {|uri(Base)||$PathTerm|},
    U = 'https://example.org/path/to/resource'.

'interpolate path (no query)' :-
    Path = 'somewhere',
    U = {|uri||example.net/$Path|},
    U = 'http://example.net/somewhere'.

'interpolate path (with query)' :-
    Path = 'path/to/resource',
    PathTerm = path/to/resource,
    A = path/to,
    B = resource,
    U = {|uri||http://example.org/$Path?a=b|},
    U = {|uri||http://example.org/$PathTerm?a=b|},
    U = {|uri||http://example.org/$A/$B?a=b|},
    U = 'http://example.org/path/to/resource?a=b'.

'interpolate entire query' :-
    Query = [a=1, b=two],
    U = {|uri||http://example.org/q?$Query|},
    U = 'http://example.org/q?a=1&b=two'.

'interpolate entire query (as a dict)' :-
    Query = _{alpha: a, beta: b},
    U = {|uri||http://example.org/q?$Query|},
    U = 'http://example.org/q?alpha=a&beta=b'.

'interpolate individual query parameters' :-
    A = one,
    B = 2,
    U = {|uri||https://example.org/q?a=$A&b=$B|},
    U = 'https://example.org/q?a=one&b=2'.
