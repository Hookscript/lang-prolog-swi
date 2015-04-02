:- module(req, []).


:- dynamic
    body/1,
    header/2,
    path_query/1,
    param/2.


%% param(+Name,+Default,-Value)
%
%  Like param/2 but allows a default value if the request
%  provides no value for Name.
param(Name,Default,Value) :-
    once( param(Name,Value); Value=Default ).
