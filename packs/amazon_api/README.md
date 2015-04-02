# Synopsis

    :- use_module(library(amazon_api)).
    ?- build_agent(Agent, ...),
       item_lookup(Agent, 'B00000DMAX', Item),
       title(Item, Title).
    Title = 'Mario Kart 64'.

# Description

Make API calls to Amazon Web Services.  This version has been tested with Amazon's [Product Advertising API](https://affiliate-program.amazon.com/gp/advertising/api/detail/main.html), but request/5 should work with other Amazon APIs too.

# Changes in this Version

  * Remove redundant URI encoding in V7.1.13 and later

# Installation

Using SWI-Prolog 6.3 or later:

    ?- pack_install(amazon_api).

This module uses [semantic versioning](http://semver.org/).

Source code available and pull requests accepted at
http://github.com/mndrix/amazon_api

@author Michael Hendricks <michael@ndrix.org>
@license BSD
