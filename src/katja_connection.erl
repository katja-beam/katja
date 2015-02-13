% Copyright (c) 2014-2015, Daniel Kempkens <daniel@kempkens.io>
%
% Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted,
% provided that the above copyright notice and this permission notice appear in all copies.
%
% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
% WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
% DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
% NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%
% @author Daniel Kempkens <daniel@kempkens.io>
% @copyright {@years} Daniel Kempkens
% @version {@version}
% @doc Handles connections to Riemann.

-module(katja_connection).

-include("katja_types.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(DEFAULT_HOST, "127.0.0.1").
-define(DEFAULT_PORT, 5555).
-define(DEFAULT_TRANSPORT, detect).
-define(TCP_MIN_SIZE, 16385).

-record(connection_state, {
  tcp_socket :: gen_tcp:socket(),
  udp_socket :: gen_udp:socket(),
  host :: string(),
  port :: pos_integer(),
  transport :: transport()
}).

% Types

-opaque state() :: #connection_state{}.

-type transport() :: detect | udp | tcp | config.

-export_type([
  state/0,
  transport/0
]).

% API
-export([
  connect/0,
  connect/2,
  connect_udp/0,
  connect_udp/2,
  connect_tcp/0,
  connect_tcp/2,
  disconnect/1,
  send_message/3
]).

% API

% @doc Tries to connect to Riemann via UDP and TCP. Delegates to {@link connect/2}.<br />
%      `Host' and `Port' are read from the application configuration.<br /><br />
%      <strong>Defaults</strong><br />
%      `Host': 127.0.0.1<br />
%      `Port': 5555
-spec connect() -> {ok, state()} | {error, term()}.
connect() ->
  Host = application:get_env(katja, host, ?DEFAULT_HOST),
  Port = application:get_env(katja, port, ?DEFAULT_PORT),
  connect(Host, Port).

% @doc Tries to connect to Riemann via UDP and TCP using the specified `Host' and `Port'.<br />
%      A default message transport can be set using the application configuration. This only affects
%      entities (events and/or states) that are sent to Riemann, since querying always uses TCP.
-spec connect(string(), pos_integer()) -> {ok, state()} | {error, term()}.
connect(Host, Port) ->
  Transport = application:get_env(katja, transport, ?DEFAULT_TRANSPORT),
  State = #connection_state{host=Host, port=Port, transport=Transport},
  case maybe_connect_udp(State) of
    {ok, State2} -> maybe_connect_tcp(State2);
    {error, _Reason}=E -> E
  end.

% @doc Tries to connect to Riemann via UDP only. Delegates to {@link connect_udp/2}.<br />
%      `Host' and `Port' are read from the application configuration.<br /><br />
%      <strong>Defaults</strong><br />
%      `Host': 127.0.0.1<br />
%      `Port': 5555
-spec connect_udp() -> {ok, state()} | {error, term()}.
connect_udp() ->
  Host = application:get_env(katja, host, ?DEFAULT_HOST),
  Port = application:get_env(katja, port, ?DEFAULT_PORT),
  connect_udp(Host, Port).

% @doc Tries to connect to Riemann via UDP using the specified `Host' and `Port'.
-spec connect_udp(string(), pos_integer()) -> {ok, state()} | {error, term()}.
connect_udp(Host, Port) ->
  State = #connection_state{host=Host, port=Port, transport=udp},
  maybe_connect_udp(State).

% @doc Tries to connect to Riemann via TCP only. Delegates to {@link connect_tcp/2}.<br />
%      `Host' and `Port' are read from the application configuration.<br /><br />
%      <strong>Defaults</strong><br />
%      `Host': 127.0.0.1<br />
%      `Port': 5555
-spec connect_tcp() -> {ok, state()} | {error, term()}.
connect_tcp() ->
  Host = application:get_env(katja, host, ?DEFAULT_HOST),
  Port = application:get_env(katja, port, ?DEFAULT_PORT),
  connect_tcp(Host, Port).

% @doc Tries to connect to Riemann via TCP using the specified `Host' and `Port'.
-spec connect_tcp(string(), pos_integer()) -> {ok, state()} | {error, term()}.
connect_tcp(Host, Port) ->
  State = #connection_state{host=Host, port=Port, transport=tcp},
  maybe_connect_tcp(State).

% @doc Disconnects all connected sockets from Riemann.<br />
%      This method detects which sockets actually exist and only disconnects the ones that do.
-spec disconnect(state()) -> ok.
disconnect(#connection_state{udp_socket=undefined, tcp_socket=undefined}) -> ok;
disconnect(#connection_state{udp_socket=Socket, tcp_socket=undefined}) -> gen_udp:close(Socket);
disconnect(#connection_state{udp_socket=undefined, tcp_socket=Socket}) -> gen_tcp:close(Socket);
disconnect(#connection_state{udp_socket=UdpSocket, tcp_socket=TcpSocket}) ->
  ok = gen_udp:close(UdpSocket),
  gen_tcp:close(TcpSocket).

% @doc Sends a message to Riemann via UDP or TCP.<br /><br />
%      `Transport' can be one of the following values:<br />
%      `config': Uses the `transport' configuration option<br />
%      `detect': Uses the UDP transport for messages up to 16Kb in size and TCP for everything larger than that<br />
%      `udp': Send the message via UDP<br />
%      `tcp': Sends the message via TCP
-spec send_message(Transport :: transport(), binary(), state()) -> {{ok, riemannpb_message()}, state()} | {{error, term()}, state()}.
send_message(config, Msg, #connection_state{transport=udp}=S) -> send_message_udp(Msg, S);
send_message(config, Msg, #connection_state{transport=tcp}=S) -> send_message_tcp(Msg, S);
send_message(config, Msg, #connection_state{transport=detect}=S) -> send_message(detect, Msg, S);
send_message(detect, Msg, State) ->
  Type = send_message_transport(Msg),
  send_message(Type, Msg, State);
send_message(udp, Msg, State) -> send_message_udp(Msg, State);
send_message(tcp, Msg, State) -> send_message_tcp(Msg, State).

% Private

-spec maybe_connect_udp(state()) -> {ok, state()} | {error, term()}.
maybe_connect_udp(State) ->
  case gen_udp:open(0, [binary, {active, false}]) of
    {ok, Socket} ->
      State2 = State#connection_state{udp_socket=Socket},
      {ok, State2};
    {error, _Reason}=E -> E
  end.

-spec maybe_connect_tcp(state()) -> {ok, state()} | {error, term()}.
maybe_connect_tcp(#connection_state{host=Host, port=Port}=S) ->
  Options = [binary, {active, false}, {nodelay, true}],
  case gen_tcp:connect(Host, Port, Options, 5000) of
    {ok, Socket} ->
      S2 = S#connection_state{tcp_socket=Socket},
      {ok, S2};
    {error, _Reason} ->
      {ok, S#connection_state{tcp_socket=undefined}}
  end.

-spec send_message_transport(binary()) -> transport().
send_message_transport(Msg) ->
  MsgSize = byte_size(Msg),
  if
    MsgSize < ?TCP_MIN_SIZE -> udp;
    true -> tcp
  end.

-spec send_message_udp(binary(), state()) -> {{ok, riemannpb_message()}, state()} | {{error, term()}, state()}.
send_message_udp(Msg, #connection_state{host=Host, port=Port, udp_socket=Socket}=S) when Socket =/= undefined ->
  case gen_udp:send(Socket, Host, Port, Msg) of
    ok ->
      RetMsg = #riemannpb_msg{ok=true},
      {{ok, RetMsg}, S};
    {error, _Reason}=E -> {E, S}
  end.

connect_and_send(Msg, S=#connection_state{host=Host, port=Port}) ->
  {ok, #connection_state{tcp_socket=NewSocket}} = connect_tcp(Host, Port),
  S2 = S#connection_state{tcp_socket=NewSocket},
  send_message_tcp(Msg, S2).

-spec send_message_tcp(binary(), state()) -> {{ok, riemannpb_message()}, state()} | {{error, term()}, state()}.
send_message_tcp(Msg, #connection_state{tcp_socket=Socket}=S) when Socket =/= undefined ->
  MsgSize = byte_size(Msg),
  Msg2 = <<MsgSize:32/integer-big, Msg/binary>>,
  case gen_tcp:send(Socket, Msg2) of
    ok ->
      case receive_reply_tcp(Socket) of
        {ok, _RetMsg}=O -> {O, S};
        {error, _Reason}=E -> {E, S}
      end;
    {error, Reason} when Reason == econnrefused orelse
                         Reason == closed orelse
                         Reason == timeout  ->
      connect_and_send(Msg, S);
    {error, _Reason}=E -> {E, S}
  end;
send_message_tcp(Msg, #connection_state{tcp_socket=undefined}=S) ->
  connect_and_send(Msg, S).

-spec receive_reply_tcp(gen_tcp:socket()) -> {ok, term()} | {error, term()}.
receive_reply_tcp(Socket) ->
  receive_reply_tcp(Socket, <<>>).

-spec receive_reply_tcp(gen_tcp:socket(), binary()) -> {ok, term()} | {error, term()}.
receive_reply_tcp(Socket, Buffer) ->
  case gen_tcp:recv(Socket, 0, 4000) of
    {ok, BinMsg} ->
      BinMsg2 = <<Buffer/binary, BinMsg/binary>>,
      case decode_message(BinMsg2) of
        too_short -> receive_reply_tcp(Socket, BinMsg2);
        #riemannpb_msg{ok=true}=Msg -> {ok, Msg};
        #riemannpb_msg{ok=false, error=Reason} -> {error, Reason}
      end;
    {error, _Reason}=E -> E
  end.

-spec decode_message(binary()) -> riemannpb_message() | too_short.
decode_message(<<MsgSize:32/integer-big, Msg/binary>>) when MsgSize > byte_size(Msg) ->
  too_short;
decode_message(<<MsgSize:32/integer-big, Msg/binary>>) ->
  case Msg of
    <<Msg2:MsgSize/binary, _Rest/binary>> -> katja_pb:decode_riemannpb_msg(Msg2);
    _ -> #riemannpb_msg{ok=false, error="Response could not be decoded"}
  end.

% Tests (private functions)

-ifdef(TEST).
disconnect_test() ->
  ?assertEqual(ok, disconnect(#connection_state{})),
  {ok, Socket} = gen_udp:open(0, [binary, {active, false}]),
  ?assertEqual(ok, disconnect(#connection_state{udp_socket=Socket})).

send_message_transport_test() ->
  BinData = unicode:characters_to_binary(lists:flatten(lists:duplicate(?TCP_MIN_SIZE, "a"))),
  ?assertEqual(udp, send_message_transport(<<>>)),
  ?assertEqual(tcp, send_message_transport(BinData)),
  ?assertEqual(udp, send_message_transport(binary:part(BinData, 0, byte_size(BinData) - 1))).

decode_message_test() ->
  ?assertEqual(too_short, decode_message(<<4:32/integer-big, "xxx">>)).
-endif.
