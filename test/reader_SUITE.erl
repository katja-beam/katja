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
% Since events from Katja have to indexed in order to be queried, the following configuration
% options are required:
%   (where (service "katja 2") index)
%   (where (service "katja 3") index)
%   (where (service "katja 4") index)

-module(reader_SUITE).

-include_lib("common_test/include/ct.hrl").

% Common Test
-export([
  all/0,
  groups/0,
  init_per_suite/1,
  init_per_group/2,
  init_per_testcase/2,
  end_per_testcase/2,
  end_per_group/2,
  end_per_suite/1
]).

% Tests
-export([
  query/1,
  query_pid/1,
  query_async/1,
  query_event/1,
  query_event_pid/1,
  query_event_async/1,
  ignore_unknown_messages/1
]).

% Common Test

all() ->
  [
    {group, normal},
    {group, process},
    {group, process_args}
  ].

groups() ->
  [
    {normal, [], [
      query,
      query_async,
      query_event,
      query_event_async
    ]},
    {process, [], [
      query_pid,
      query_event_pid,
      ignore_unknown_messages
    ]},
    {process_args, [], [
      query_pid,
      query_event_pid,
      ignore_unknown_messages
    ]}
  ].

init_per_suite(Config) ->
  ok = katja:start(),
  ok = setup_events(),
  Config.

init_per_group(Group, Config) -> [{reader_type, Group} | Config].

init_per_testcase(_Test, Config) ->
  case ?config(reader_type, Config) of
    process ->
      {ok, RPid} = katja_reader:start_link(),
      [{pid_reader, RPid} | Config];
    process_args ->
      {ok, RPid} = katja_reader:start_link([{host, "127.0.0.1"}, {port, 5555}]),
      [{pid_reader, RPid} | Config];
    _ -> Config
  end.

end_per_testcase(_Test, Config) ->
  case ?config(reader_type, Config) of
    Type when Type == process, Type == process_args ->
      RPid = ?config(pid_reader, Config),
      ok = katja_reader:stop(RPid),
      ok;
    _ -> ok
  end.

end_per_group(_Group, _Config) -> ok.

end_per_suite(_Config) ->
  ok = katja:stop(),
  ok.

% Tests

query(_Config) ->
  {ok, [ServiceEvent]} = katja:query("service = \"katja 2\" and tagged \"query1\""),
  {metric, 9001} = lists:keyfind(metric, 1, ServiceEvent),
  {ok, [AttrEvent]} = katja:query("service = \"katja 3\" and metric = 9002 and tagged \"query2\""),
  {attributes, [{"foo", "bar"}]} = lists:keyfind(attributes, 1, AttrEvent),
  {ok, [MultiTagEvent]} = katja:query("service = \"katja 4\" and tagged \"query3\" and tagged \"query4\""),
  {metric, 9003} = lists:keyfind(metric, 1, MultiTagEvent).

query_pid(Config) ->
  RPid = ?config(pid_reader, Config),
  {ok, [ServiceEvent]} = katja:query(RPid, "service = \"katja 2\" and tagged \"query1\""),
  {metric, 9001} = lists:keyfind(metric, 1, ServiceEvent),
  {ok, [AttrEvent]} = katja:query(RPid, "service = \"katja 3\" and metric = 9002 and tagged \"query2\""),
  {attributes, [{"foo", "bar"}]} = lists:keyfind(attributes, 1, AttrEvent),
  {ok, [MultiTagEvent]} = katja:query(RPid, "service = \"katja 4\" and tagged \"query3\" and tagged \"query4\""),
  {metric, 9003} = lists:keyfind(metric, 1, MultiTagEvent).

query_async(_Config) ->
  Ref = katja:query_async("service = \"katja 2\" and tagged \"query1\""),
  receive
    {Ref, {ok, [ServiceEvent]}} ->
      {metric, 9001} = lists:keyfind(metric, 1, ServiceEvent)
  end.

query_event(_Config) ->
  {ok, [ServiceEvent]} = katja:query_event([{service, "katja 2"}, {tags, ["query1"]}]),
  {metric, 9001} = lists:keyfind(metric, 1, ServiceEvent),
  {ok, [AttrEvent]} = katja:query_event([{service, "katja 3"}, {metric, 9002}, {tags, ["query2"]}]),
  {attributes, [{"foo", "bar"}]} = lists:keyfind(attributes, 1, AttrEvent),
  {ok, [MultiTagEvent]} = katja:query_event([{service, "katja 4"}, {tags, ["query3", "query4"]}]),
  {metric, 9003} = lists:keyfind(metric, 1, MultiTagEvent).

query_event_pid(Config) ->
  RPid = ?config(pid_reader, Config),
  {ok, [ServiceEvent]} = katja:query_event(RPid, [{service, "katja 2"}, {tags, ["query1"]}]),
  {metric, 9001} = lists:keyfind(metric, 1, ServiceEvent),
  {ok, [AttrEvent]} = katja:query_event(RPid, [{service, "katja 3"}, {metric, 9002}, {tags, ["query2"]}]),
  {attributes, [{"foo", "bar"}]} = lists:keyfind(attributes, 1, AttrEvent),
  {ok, [MultiTagEvent]} = katja:query_event(RPid, [{service, "katja 4"}, {tags, ["query3", "query4"]}]),
  {metric, 9003} = lists:keyfind(metric, 1, MultiTagEvent).

query_event_async(_Config) ->
  Ref = katja:query_event_async([{service, "katja 2"}, {tags, ["query1"]}]),
  receive
    {Ref, {ok, [ServiceEvent]}} ->
      {metric, 9001} = lists:keyfind(metric, 1, ServiceEvent)
  end.

ignore_unknown_messages(Config) ->
  RPid = ?config(pid_reader, Config),
  ignored = gen_server:call(RPid, foobar),
  ok = gen_server:cast(RPid, foobar),
  RPid ! foobar.

% Private

setup_events() ->
  ok = katja:send_event([{service, "katja 2"}, {metric, 9001}, {tags, ["query1"]}]),
  ok = katja:send_event([{service, "katja 3"}, {metric, 9002}, {tags, ["query2"]}, {attributes, [{"foo", "bar"}]}]),
  ok = katja:send_event([{service, "katja 4"}, {metric, 9003}, {tags, ["query3", "query4"]}, {description, "test"}]),
  ok = timer:sleep(100),
  ok.
