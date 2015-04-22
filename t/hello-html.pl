:- use_module(library(hookscript)).

hook :-
    req:param(whom,world,Whom),
    assert(res:header(content_type,text/html)),
    template(Template),
    format(Template,[Whom]).

template("
<html>
<body>
    <h1>Hello, ~w!</h1>

    <form method=GET>
        <input type=submit value='Say Hello to' />
        <input type=text name=whom placeholder='world' />
    </form>
</body>
</html>
").
