:- use_module(library(hookscript)).
:- use_module(library(web), []).

hook :-
    req:param(protocol, Http),
    req:param(file, File),
    format(atom(Url),"~s://storage.googleapis.com/hookscript/~w", [Http,File]),
    web:get(Url,[status_code(Status),codes(Codes)]),
    ( Status = 200 ->
        format("~s",[Codes])
    ; otherwise ->
        res:assert(status(Status, "")),
        format("Request to ~s failed", [Url])
    ).
