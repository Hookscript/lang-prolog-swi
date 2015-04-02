:- use_module(library(hookscript)).

hook :-
    req:param(resource, Resource),
    hog(Resource).

hog(cpu) :-
    hog(cpu).
hog(mem) :-
    hog_mem([]).
hog(disk) :-
    open(junk,write,File),
    tell(File),
    forall(repeat,writeln(junk)).
hog(output) :-
    forall(between(1,1_000_000,_),writeln(junk)).


hog_mem(T) :-
    hog_mem([x|T]).
