# Prolog Reference

See also Hookscript's [general documentation](http://docs.hookscript.com/)

## Hookscript module

Each Prolog script should begin with `:- use_module(library(hookscript)).`.  Importing this
module does the following:

  * loads `req` module representing the incoming HTTP request
  * loads `res` module representing the script's HTTP response
  * loads `state` module whose facts are retained across script invocations

## HTTP Request

An incoming HTTP request is represented by predicates in the `req` module:

  * `body(-Body:stream)` - a stream from which to read the HTTP request body
  * `header(?Name:atom,?Value:atom)` - true if an HTTP header with `Name` has `Value`
  * `method(?Method:atom)` - an HTTP method like `get` or `post`
  * `param(?Name:atom,?Value:atom)` - true if HTTP request has a parameter `Name` with `Value`
  * `param(+Name:atom,+Default:atom,-Value:atom)` - like param/3 but allows a default value
  * `path(?Path:atom)` - path of the HTTP request

## HTTP Response

A script doesn't need to explicitly work with the `res` module.  Anything sent to the currently selected
output stream becomes the HTTP response body.  However, you can use predicates in `res` to have greater
control over the response.  Predicates include:

  * `body(+Body:string)` - assert this fact to generate a response body manually
  * `header(+Name,+Value)` - assert these facts to add response headers
  * `status(+Code:integer,+Message:string)` - assert this fact to set the response status like `res:assert(status(204,"No Content"))`

## State arguments

If your script defines `hook/0` then you should assert/retract facts in the `state` module if you want
them retained across script invocations.

Sometimes it's more natural to retain state as a single Prolog value.  In that case, you can define
hook/2.  The first argument is the state when your script starts.  You should bind the second
argument to the value that you want stored when your script finishes.

On your script's first invocation (when no state exists), the first argument is an unbound variable. You
can set a default value with `ignore(X=0)` which binds X if it's unbound but otherwise succeeds silently.

For example,

```prolog
% a script to count how many times it has been invoked
hook(N0, N) :-
    ignore(N0=0),  % initial state value should be 0
    succ(N0, N),
    writeln(N).
```

