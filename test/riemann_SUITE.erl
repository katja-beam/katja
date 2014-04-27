% Copyright (c) 2014, Daniel Kempkens <daniel@kempkens.io>
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
% Since events from <em>katja</em> have to indexed in order to be queried, the following configuration
% options are required:
%   (where (service "katja 1") index)
%   (where (service "katja 2") index)

-module(riemann_SUITE).

-include_lib("common_test/include/ct.hrl").

% Common Test
-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1
]).

% Tests
-export([
  send_event/1,
  send_state/1,
  query/1
]).

% Common Test

all() ->
  [
    send_event,
    send_state,
    query
  ].

init_per_suite(Config) ->
  ok = application:start(katja),
  Config.

end_per_suite(_Config) ->
  ok = application:stop(katja),
  ok.

% Tests

send_event(_Config) ->
  ok = katja:send_event([{service, "katja 1"}, {metric, 9001}]),
  Description = lists:flatten(lists:duplicate(4096, "abcd")),
  ok = katja:send_event([{service, "katja 1"}, {metric, 9001}, {description, Description}]),
  ok = katja:send_event([{service, "katja 2"}, {metric, 9002}, {attributes, [{"foo", "bar"}]}]).

send_state(_Config) ->
  ok = katja:send_state([{service, "katja 1"}, {state, "testing"}]).

query(_Config) ->
  {ok, [ServiceEvent]} = katja:query("service = \"katja 1\""),
  {metric, 9001} = lists:keyfind(metric, 1, ServiceEvent),
  {ok, [AttrEvent]} = katja:query("service = \"katja 2\""),
  {attributes, [{"foo", "bar"}]} = lists:keyfind(attributes, 1, AttrEvent).
