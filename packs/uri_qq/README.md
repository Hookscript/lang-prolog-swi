# Synopsis

    :- use_module(library(uri_qq)).
    :- use_module(library(http/http_client)).
    main :-
        Path = search,
        Params = [q='prolog is awesome'],
        http_get({|uri||google.com/$Path?$Params|}, Content, []),
        write(Content).

# Description

This module makes it easy to build complex URIs out of component
parts.  All necessary escaping is done automatically.  In nearly all
cases, this is more convenient than using library(uri) directly.

In most circumstances, you can just use `$Name` anywhere inside the
URI and the runtime value of variable `Name` will be inserted at that
location.  library(uri_qq) also supports the following less obvious
features:

## Suffix References

It quickly gets annoying to type and read `http://` all over the
place.  If you omit the scheme in a URI template, `http://` is
inserted automatically.  See the Synopsis above.

Also see Relative References below.

## Query interpolation

There are two ways to build dynamic queries for a URI.  The first
way is to interpolate a key or a value directly in the URI template:

    Name = name,
    Value = value,
    U = {|uri||example.com/q?name=$Value|},
    U = {|uri||example.com/q?$Name=value|},

The second way is to build a dict or a list of `Key=Value` pairs and interpolate
the entire query.  This example builds the same `U` value as above:

    Query = _{name: value},
    U = {|uri||example.com/q?$Query|},

## Relative References

Sometimes you need to create many URIs relative to a single base URI.
To avoid repeating content and to get all the convenience you
expect from relative URIs, you can specify a local base URI as the
first argument of the quasiquoter like this:

    Base = 'http://www.example.org/path/to/',
    U1 = {|uri(Base)||foo#1|},
    U2 = {|uri(Base)||foo#2|},
    U3 = {|uri(Base)||../goes/elsewhere|},

When it encounters a relative URI in the clause, it's resolved against
the base URI to produce an absolute URI.

# Changes in this Version

  * Interpolate path terms like `a/b/c` instead of just atoms
  * Larger test suite

# Installation

Using SWI-Prolog 6.3.16 or later:

    ?- pack_install(uri_qq).

For repository and pull requests, see https://github.com/mndrix/uri_qq

This module uses [semantic versioning](http://semver.org/).

@author Michael Hendricks <michael@ndrix.org>
@license BSD
