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
% This suite expects Riemann to be running on 127.0.0.1:5555.

-module(connection_SUITE).

-include_lib("common_test/include/ct.hrl").

% Common Test
-export([
  all/0,
  init_per_testcase/2,
  end_per_testcase/2
]).

% Tests
-export([
  connect/1,
  connect_tcp/1,
  connect_udp/1,
  send_invalid_tcp/1,
  send_invalid_config_tcp/1,
  send_invalid_config_udp/1
]).

% Common Test

all() ->
  [
    connect,
    connect_tcp,
    connect_udp,
    send_invalid_tcp,
    send_invalid_config_tcp,
    send_invalid_config_udp
  ].

init_per_testcase(send_invalid_config_tcp, Config) ->
  ok = application:set_env(katja, transport, tcp),
  Config;
init_per_testcase(send_invalid_config_udp, Config) ->
  ok = application:set_env(katja, transport, udp),
  Config;
init_per_testcase(_Test, Config) ->
  Config.

end_per_testcase(Test, _Config) when Test == send_invalid_config_tcp; Test == send_invalid_config_udp  ->
  application:unset_env(katja, transport);
end_per_testcase(_Test, _Config) ->
  ok.

% Tests

connect(_Config) ->
  {ok, State} = katja_connection:connect(),
  ok = katja_connection:disconnect(State),
  {ok, ErrorState} = katja_connection:connect("10.99.99.99", 9001),
  undefined = element(2, ErrorState).

connect_tcp(_Config) ->
  {ok, State} = katja_connection:connect_tcp(),
  ok = katja_connection:disconnect(State),
  {ok, ErrorState} = katja_connection:connect_tcp("10.99.99.99", 9001),
  undefined = element(2, ErrorState).

connect_udp(_Config) ->
  {ok, State} = katja_connection:connect_udp(),
  ok = katja_connection:disconnect(State).

send_invalid_tcp(_Config) ->
  {ok, State} = katja_connection:connect_tcp(),
  {{error, closed}, State2} = katja_connection:send_message(tcp, <<"invalid">>, State),
  {{error, closed}, State3} = katja_connection:send_message(tcp, <<"still invalid">>, State2),
  ok = katja_connection:disconnect(State3).

send_invalid_config_tcp(_Config) ->
  {ok, State} = katja_connection:connect(),
  {{error, closed}, State2} = katja_connection:send_message(config, <<"invalid">>, State),
  {{error, closed}, State3} = katja_connection:send_message(config, iolist_to_binary(lists:duplicate(5120, "abcd")), State2),
  ok = katja_connection:disconnect(State3).

send_invalid_config_udp(_Config) ->
  {ok, State} = katja_connection:connect(),
  {{ok, _Rep}, State2} = katja_connection:send_message(config, <<"invalid">>, State),
  {{error, emsgsize}, State3} = katja_connection:send_message(config, iolist_to_binary(lists:duplicate(5120, "abcd")), State2),
  {{ok, _Rep}, State4} = katja_connection:send_message(config, <<"invalid">>, State3),
  ok = katja_connection:disconnect(State4).
