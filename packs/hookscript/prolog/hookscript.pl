:- module(hookscript, [main/1]).

:- use_module(library(hookscript/req)).

:- use_module(library(http/http_client), [http_read_data/3]).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_open), [http_open/3]).
:- use_module(library(sweet)).


:- dynamic
    res:status/2,
    res:header/2,
    res:body/1.


main(_) :-
    once(parse_request),
    call_hook,
    send_response.

parse_request :-
    open("request",read,Input),
    cleanup(close(Input)),
    http_read_request(Input,Terms),
    maplist(assert_request(Terms),Terms).

% how do request terms map to request database facts
assert_request(Request,input(_)) :-
    http_read_data(Request,Body,[]),
    ( is_html_form(Request) ->
        assert_request(Request,search(Body))
    ; otherwise ->
        req:assert(body(Body))
    ).
assert_request(_,search(QueryParams)) :-
    maplist(assert_query, QueryParams).
assert_request(_,request_uri(Uri)) :-
    req:assert(path_query(Uri)).
assert_request(_,Attr) :-
    special_request_attribute(Attr),
    req:assert(Attr).
assert_request(_,Header) :-
    ( Header =.. [Name, Value] ->
        req:assert(header(Name,Value))
    ; otherwise ->
        throw(bad_header_form(Header))
    ).

assert_query(Key=ValueAtom) :-
    atom_codes(ValueAtom, ValueCodes),
    name(Value, ValueCodes),
    req:assert(param(Key, Value)).

is_html_form(Request) :-
    member(content_type('application/x-www-form-urlencoded'), Request).

% these request attributes are not HTTP headers
special_request_attribute(method(_)).
special_request_attribute(path(_)).
special_request_attribute(http_version(_)).


%% restore_state_to_module
%
%  Loads "state" file into state module.
restore_state_to_module :-
    exists_file(state),
    !,
    load_files(state,[]).
restore_state_to_module.  % state file is missing


%% save_state_module_to_file
%
%  Serialize the state module to a file for later loading
%  by restore_state_to_module.
save_state_module_to_file :-
    tell(state),
    write(:- module(state,[])),
    write('.'),
    nl,
    state:listing,
    told.


%% restore_state_to_value
%
%  Loads "state" file into Prolog logic variable.
restore_state_to_value(Val) :-
    exists_file(state),
    !,
    see(state),
    read_term(Val,[]),
    seen.
restore_state_to_value(_).  % state file is missing


%% save_state_value_to_file
%
%  Serialize a value to a file for later loading
%  by restore_state_to_value.
save_state_value_to_file(Val) :-
    nonvar(Val),
    !,
    tell(state),
    write_term(Val,[quoted(true),fullstop(true),nl(true)]),
    told.
save_state_value_to_file(_) :-  % unbound state value
    ( exists_file(state) -> delete_file(state); true ).


%% call_hook
%
%  Call user's hook implementation.
call_hook :-
    with_output_to(string(Content), call_hook_),
    ( res:body(_) -> true ; res:assert(body(Content)) ),
    populate_response_defaults.

call_hook_ :-
    % which predicate defines the user's code?
    ( current_predicate(user:hook/0) ->
        restore_state_to_module,
        user:hook,
        save_state_module_to_file
    ; current_predicate(user:hook/2) ->
        restore_state_to_value(State0),
        user:hook(State0,State),
        save_state_value_to_file(State)
    ; otherwise ->
        throw("Missing hook predicate")
    ).


%% populate_response_defaults
%
%  Assert default values in res module if the hook didn't
%  do it already.
populate_response_defaults :-
    forall( response_default(Template,Fact)
          , ( res:Template -> true ; res:assert(Fact) )
          ).

response_default(status(_,_), status(200,"OK")).
response_default( header(content_type,_)
                , header(content_type,text/plain)
                ).
response_default( header(access_control_allow_origin,_)
                , header(access_control_allow_origin,"*")
                ).
response_default( header(server,_)
                , header(server,Server)
                ) :-
    current_prolog_flag(version, Version),
    format(string(Server), "hookscript/prolog-swi-~d", [Version]).


%% send_response
%
%  Print an HTTP response based on facts present in res module.
send_response :-
    open("response",write,Stream),
    cleanup(close(Stream)),
    with_output_to(Stream,send_response_).

send_response_ :-
    % output status line
    res:status(Code, Message),
    format("HTTP/1.1 ~d ~s\r~n", [Code,Message]),

    % output headers
    forall( res:header(Name,Value)
          , print_header(Name, Value)
          ),

    % output content
    format("\r~n"),
    res:body(Content),
    write(Content).


%% print_header(+Name:atom, +Value:text)
%
%  Print a single HTTP header.
print_header(Name, Value) :-
    phrase(http_header:header_field(Name,Value),Codes),
    format("~s", [Codes]).
