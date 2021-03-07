-module(chat).
-compile(export_all).
-compile({no_auto_import,[register/2]}).

start() ->
    gen_server:start_link({local, chat_utils}, chat_utils, [], []),
    socket_utils:start_link(5678, {127,0,0,1}).

beginLoop(Socket) ->
    io:fwrite("BeginLoop~n"),
    {ok, Bin} = gen_tcp:recv(Socket, 0),
    io:fwrite("received Msg~n"),
    Message = binary_to_list(Bin),
    {"register", [_Comma | Name]} = lists:splitwith(fun(C) -> C =/= $, end, Message),
    tryRegister(Name, Socket).


loop(Name, Socket) ->
%%    io:fwrite("MainLoop~n"),
    {ok, Bin} = gen_tcp:recv(Socket, 0),
%%    io:fwrite("received Msg~n"),
    Message = binary_to_list(Bin),
%%    io:fwrite("message: ~p~n", [Message]),

    {Command, Rest} = lists:splitwith(fun(C) -> C =/= $, end, Message),
    case Command of
        "send" ->
            [_Comma | Msg] = Rest,
            {Sender, [_Comma | Content]} = lists:splitwith(fun(C) -> C =/= $, end, Msg),
            gen_server:cast(chat_utils, {send, Sender, Content}),
            loop(Name, Socket);
        "quit" ->
            deregistered = gen_server:call(chat_utils, {deregister, Name}),
            gen_tcp:send(Socket, "Goodbye"),
            ok
    end.


tryRegister(Name, Socket) ->
    Response = gen_server:call(chat_utils, {register, Name, Socket}),
    case Response of
        taken ->
            gen_tcp:send(Socket, "register;error, Your Name is already taken"),
            ok;
        welcome ->
            gen_tcp:send(Socket, "register;ok, You are registered. Welcome!"),
            loop(Name, Socket)
    end.

