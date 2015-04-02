:- module(uri_qq, [uri/4]).
:- use_module(library(apply), [maplist/3]).
:- use_module(library(quasi_quotations)).
:- use_module(library(readutil), [read_stream_to_codes/2]).
:- use_module(library(record)).
:- use_module(library(uri), [uri_components/2, uri_data/3]).


% We parse the quasiquotation content into a URI term, replace $-escaped
% variables with their values, then convert the URI term back into an
% atom. The round trip makes sure that all necessary escaping
% and normalization is done properly.
%
% I wanted to use library(uri) directly, but it leaves too many of the
% URI's components hidden inside opaque atoms (authority, path segments,
% query name-value pairs). The uriqq record splits everything down to
% the smallest structural level.

% represents a URI with more structure than library(uri) provides
:- record uriqq( scheme
               , user
               , password
               , host
               , port
               , path
               , search
               , fragment
               ).

% atom_uri(+Atom, -Uri)
% atom_uri(-Atom, +Uri)
%
% True if Atom represents the structured Uri.
atom_uri(Atom, UriQQ) :-
    var(Atom),
    uri_uriqq(Uri, UriQQ),
    uri_components(Atom, Uri).
atom_uri(Atom, UriQQ) :-
    atom(Atom),
    uri_components(Atom, Uri),
    uri_uriqq(Uri, UriQQ).


% describe relation between uri_components and uriqq terms
uri_uriqq(Uri, UriQQ) :-
    scheme(Uri, UriQQ),
    authority(Uri, UriQQ),
    path(Uri, UriQQ),
    search(Uri, UriQQ),
    fragment(Uri, UriQQ).

scheme(Uri, UriQQ) :-
    uri_data(scheme, Uri, Scheme),
    uriqq_data(scheme, UriQQ, Scheme).

authority(Uri, UriQQ) :-
    uri_data(authority, Uri, Authority),
    uri_authority_data(user,     As, User),
    uri_authority_data(password, As, Password),
    uri_authority_data(host,     As, Host),
    uri_authority_data(port,     As, Port),

    uriqq_data(user,     UriQQ, User),
    uriqq_data(password, UriQQ, Password),
    uriqq_data(host,     UriQQ, Host),
    uriqq_data(port,     UriQQ, Port),

    ( var(Authority), var(User), var(Password), var(Host), var(Port)
    ; uri_authority_components(Authority, As)
    ).

path(Uri, UriQQ) :-
    uri_data(path, Uri, PathA),
    uriqq_data(path, UriQQ, PathB),
    ( var(PathA), var(PathB)
    ; nonvar(PathB),
      maplist(path_term,PathC,PathB),
      atomic_list_concat(PathC,/,PathA)
    ; atomic_list_concat(PathB,/,PathA)
    ).

search(Uri, UriQQ) :-
    uri_data(search, Uri, Search),
    uriqq_data(search, UriQQ, Pairs),
    ( var(Search), var(Pairs)
    ; atom(Search), atom_concat('$', _, Search), Pairs=Search
    ; is_dict(Pairs),
      dict_pairs(Pairs, _, Pairs1),
      uri_query_components(Search, Pairs1)
    ; uri_query_components(Search, Pairs)
    ).

fragment(Uri, UriQQ) :-
    uri_data(fragment, Uri, Fragment),
    uriqq_data(fragment, UriQQ, Fragment).

replace_variables(Vars, Term0, Term) :-
    % $-prefixed variable needing substitution
    atom(Term0),
    atom_concat('$', Name, Term0),
    !,
    ( memberchk(Name=Value, Vars) ->
        Term = Value
    ; % otherwise ->
        Term = Term0
    ).
replace_variables(Vars, Term0, Term) :-
    % compound term needing recursive replacement
    nonvar(Term0),
    Term0 =.. [Name|Args0],
    !,
    maplist(replace_variables(Vars), Args0, Args),
    Term =.. [Name|Args].
replace_variables(_, Term, Term) :-
    % leave everything else alone
    true.

path_term(Path, Term) :-
    var(Path),
    Term = _/_,
    !,
    list_slashes(PathList, Term),
    atomic_list_concat(PathList, /, Path).
path_term(Path, Term) :-
    atom(Term),
    Path = Term.

% relate a list to a slash-separated term
list_slashes(List, Slashes) :-
    nonvar(List),
    !,
    reverse(List, ReverseList),
    list_slashes_(ReverseList, Slashes).
list_slashes(List, Slashes) :-
    nonvar(Slashes),
    list_slashes_(ReverseList, Slashes),
    reverse(ReverseList, List).

list_slashes_([Tail|Path], Head/Tail) :-
    Path \== [],
    !,
    list_slashes_(Path, Head).
list_slashes_([X], X).

% parse quasiquotation into a result
qq(Stream, Vars, MaybeBase, Result) :-
    read_stream_to_codes(Stream, Codes),
    atom_codes(Atom, Codes),
    qq_an_atom(Atom, Vars, UriQQ),
    uriqq_data(scheme, UriQQ, Scheme),
    ( var(Scheme) ->
        ( MaybeBase = just(Base) ->
            Result = uri_relative(Base, UriQQ)
        ; % otherwise ->
            Result = uri_suffix(UriQQ)
        )
    ; % otherwise ->
        Result = uri_absolute(UriQQ)
    ).

qq_an_atom(Atom, Vars, Result) :-
    atom_uri(Atom, Uri0),
    replace_variables(Vars, Uri0, Result).

:- quasi_quotation_syntax(uri).
uri(Content,Args,Vars,Result) :-
    ( Args = [Base] ->
        MaybeBase = just(Base)
    ; % otherwise ->
        MaybeBase = none
    ),
    with_quasi_quotation_input(Content, Stream, qq(Stream,Vars,MaybeBase,Result)).


:- use_module(library(function_expansion)).
user:function_expansion( uri_absolute(UriQQ)
                       , Atom
                       , once(uri_qq:atom_uri(Atom,UriQQ))
                       ).
user:function_expansion( uri_relative(Base, UriQQ)
                       , Atom
                       , ( once(uri_qq:atom_uri(RelUri,UriQQ))
                         , uri_resolve(RelUri, Base, Atom)
                         )
                       ).
user:function_expansion( uri_suffix(UriQQ)
                       , Atom
                       , ( once(uri_qq:atom_uri(Suffix,UriQQ))
                         , atom_concat('http://', Suffix, Atom)
                         )
                       ).
