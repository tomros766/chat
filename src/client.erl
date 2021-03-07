-module(client).
-compile(export_all).
-compile({no_auto_import,[register/2]}).



start() -> spawn(?MODULE, init, []).

init() ->
  SomeHostInNet = {127,0,0,1}, % to make it runnable on one machine
    {ok, Sock} = gen_tcp:connect(SomeHostInNet, 5678, 
                                 [binary, {packet, 0}, {active, true}]),
    loop(Sock).


loop(Socket, Name) ->
  receive
    {send, Message} ->
%%      io:fwrite("sending~n"),
      send(Message, Name, Socket),
      loop(Socket, Name);
    {tcp, _Sender, Msg} ->
      {"from", [_semicolon | Content]} = lists:splitwith(fun(C) -> C =/= $; end, binary_to_list(Msg)),
%%      [SenderName, Message] = lists:splitwith()
      io:fwrite("~s~n", [Content]),
      loop(Socket, Name)
end.


loop(Socket) ->
  receive
    {register, Name} ->
%%      io:fwrite("registering~n"),
      Response = register(Name, Socket),
      case Response of
        ok ->
          loop(Socket, Name);
        {error, taken} ->
          loop(Socket)
      end
  end. 


register(Name, Socket) ->
    gen_tcp:send(Socket, list_to_binary("register," ++ Name)),
    receive
      {tcp, Sender, Any} ->
        io:fwrite("Hello ~s, you have been succesfully registered to the conversation!~n", [Name]),
        ok;
      {tcp, Sender, "response,taken"} ->
        io:write("Your name is already taken, choose another one to register properly~n"),
        {error, taken};
        Sth ->
          Sth
    end.

send(Message, Name, Socket) ->
  gen_tcp:send(Socket, list_to_binary("send,"++Name++","++Message)).
%%  io:fwrite("sent~n").
