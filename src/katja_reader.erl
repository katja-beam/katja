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
% @author Daniel Kempkens <daniel@kempkens.io>
% @copyright {@years} Daniel Kempkens
% @version {@version}
% @doc The `katja_reader' module is responsible for querying Riemann.<br />
%      Riemann currently does not provide real documentation on how to write queries, but there are a couple of resources
%      that can help you get started with writing them:
%      <ul>
%        <li><a href="https://github.com/aphyr/riemann/blob/master/src/riemann/Query.g" target="_blank">Grammar</a></li>
%        <li><a href="https://github.com/aphyr/riemann/blob/master/test/riemann/query_test.clj" target="_blank">Test Suite</a></li>
%      </ul>

-module(katja_reader).
-behaviour(gen_server).

-include("katja_types.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(EVENT_FIELDS, [time, state, service, host, description, tags, ttl, metric]).
-define(PB_EVENT_FIELDS, [time, state, service, host, description, tags, ttl, attributes, metric]).

% API
-export([
  start_link/0,
  start_link/1,
  stop/1,
  query/2,
  query_event/2
]).

% gen_server
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

% API

% @doc Starts a querying server process.
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
  gen_server:start_link(?MODULE, [], []).

% @doc Starts a querying server process and registers it as `{@module}'.
-spec start_link(register) -> {ok, pid()} | ignore | {error, term()}.
start_link(register) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% @doc Stops a querying server process.
-spec stop(katja:process()) -> ok.
stop(Pid) ->
  gen_server:call(Pid, terminate).

% @doc Sends a query string to Riemann and returns a list of matching events.<br />
%      Example queries can be found in the
%      <a href="https://github.com/aphyr/riemann/blob/master/test/riemann/query_test.clj" target="_blank">Riemann test suite</a>.
-spec query(katja:process(), iolist()) -> {ok, [katja:event()]} | {error, term()}.
query(Pid, Query) ->
  Msg = create_query_message(Query),
  gen_server:call(Pid, {query, Msg}).

% @doc Takes an event and transforms it into a query string. The generated query string is passed to {@link query/1}.<br />
%      Querying `attributes' is currently not supported, because Riemann itself does not provide a way to query events
%      based on `attributes'.<br /><br />
%      Converting `Event' to a query string happens inside the process calling this function.
-spec query_event(katja:process(), katja:event()) -> {ok, [katja:event()]} | {error, term()}.
query_event(Pid, Event) ->
  Query = create_query_string(Event),
  query(Pid, Query).

% gen_server

% @hidden
init([]) ->
  {ok, State} = katja_connection:connect_tcp(),
  {ok, State}.

% @hidden
handle_call({query, Msg}, _From, State) ->
  {Reply, State2} = send_message(Msg, State),
  {reply, Reply, State2};
handle_call(terminate, _From, State) ->
  {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

% @hidden
handle_cast(_Msg, State) ->
  {noreply, State}.

% @hidden
handle_info(_Msg, State) ->
  {noreply, State}.

% @hidden
terminate(normal, State) ->
  ok = katja_connection:disconnect(State),
  ok.

% @hidden
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% Private

-spec create_query_message(iolist()) -> riemannpb_message().
create_query_message(Query) ->
  #riemannpb_msg{
     query=#riemannpb_query{string=Query}
  }.

-spec create_query_string(katja:event()) -> iolist().
create_query_string(Event) ->
  Query = lists:foldr(fun(K, Q) ->
    case lists:keyfind(K, 1, Event) of
      {K, V} ->
        Q2 = add_query_field(K, V, Q),
        [" and " | Q2];
      false -> Q
    end
  end, [], ?EVENT_FIELDS),
  tl(Query).

-spec add_query_field(atom(), term(), iolist()) -> iolist().
add_query_field(time, V, Q) -> ["time = ", io_lib:format("~p", [V]) | Q];
add_query_field(state, V, Q) -> ["state = ", "\"", V, "\"" | Q];
add_query_field(service, V, Q) -> ["service = ", "\"", V, "\"" | Q];
add_query_field(host, V, Q) -> ["host = ", "\"", V, "\"" | Q];
add_query_field(description, V, Q) -> ["description = ", "\"", V, "\"" | Q];
add_query_field(tags, V, Q) ->
  TaggedQuery = ["tagged \"" ++ Tag ++ "\"" || Tag <- V],
  [string:join(TaggedQuery, " and ") | Q];
add_query_field(ttl, V, Q) -> ["ttl = ", io_lib:format("~p", [V]) | Q];
add_query_field(metric, V, Q) when is_integer(V) ->["metric = ", io_lib:format("~p", [V]) | Q];
add_query_field(metric, V, Q) -> ["metric_f = ", io_lib:format("~p", [V]) | Q].

-spec send_message(riemannpb_message(), katja_connection:state()) -> {{ok, [katja:event()]}, katja_connection:state()} | {{error, term()}, katja_connection:state()}.
send_message(Msg, State) ->
  Msg2 = katja_pb:encode_riemannpb_msg(Msg),
  BinMsg = iolist_to_binary(Msg2),
  case katja_connection:send_message(tcp, BinMsg, State) of
    {{ok, RetMsg}, State2} ->
      Events = RetMsg#riemannpb_msg.events,
      Events2 = [event_to_proplist(E) || E <- Events],
      Reply = {ok, Events2},
      {Reply, State2};
    {{error, Reason}, State2} -> {{error, Reason}, State2}
  end.

-spec event_to_proplist(riemannpb_event()) -> katja:event().
event_to_proplist(PbEvent) ->
  lists:foldr(fun(Field, Acc) ->
    set_event_field(Field, PbEvent, Acc)
  end, [], ?PB_EVENT_FIELDS).

-spec set_event_field(atom(), riemannpb_event(), katja:event()) -> katja:event().
set_event_field(time, #riemannpb_event{time=V}, E) when V =/= undefined -> [{time, V} | E];
set_event_field(state, #riemannpb_event{state=V}, E) when V =/= undefined -> [{state, V} | E];
set_event_field(service, #riemannpb_event{service=V}, E) when V =/= undefined -> [{service, V} | E];
set_event_field(host, #riemannpb_event{host=V}, E) when V =/= undefined -> [{host, V} | E];
set_event_field(description, #riemannpb_event{description=V}, E) when V =/= undefined -> [{description, V} | E];
set_event_field(tags, #riemannpb_event{tags=V}, E) when length(V) > 0 -> [{tags, V} | E];
set_event_field(ttl, #riemannpb_event{ttl=V}, E) when V =/= undefined -> [{ttl, V} | E];
set_event_field(attributes, #riemannpb_event{attributes=V}, E) when length(V) > 0 ->
  Attrs = [{AKey, AVal} || {riemannpb_attribute, AKey, AVal} <- V],
  [{attributes, Attrs} | E];
set_event_field(metric, #riemannpb_event{metric_sint64=V}, E) when V =/= undefined -> [{metric, V} | E];
set_event_field(metric, #riemannpb_event{metric_f=V}, E) when V =/= undefined -> [{metric, V} | E];
set_event_field(_Field, _PbEvent, Event) -> Event.

% Tests (private functions)

-ifdef(TEST).
create_query_message_test() ->
  ?assertMatch(#riemannpb_msg{query=#riemannpb_query{string="foo"}}, create_query_message("foo")).

create_query_string_test() ->
  F = fun(V) -> lists:flatten(V) end,
  ?assertEqual("service = \"katja\"", F(create_query_string([{service, "katja"}]))),
  ?assertEqual("service = \"katja\" and tagged \"test\"", F(create_query_string([{service, "katja"}, {tags, ["test"]}]))),
  ?assertEqual("service = \"katja\" and metric = 1", F(create_query_string([{service, "katja"}, {metric, 1}]))),
  ?assertEqual("service = \"katja\" and metric_f = 1.0", F(create_query_string([{service, "katja"}, {metric, 1.0}]))),
  ?assertEqual("state = \"testing\" and host = \"erlang\"", F(create_query_string([{state, "testing"}, {host, "erlang"}]))),
  ?assertEqual("time = 0 and description = \"foobar\" and ttl = 60.0", F(create_query_string([{time, 0}, {ttl, 60.0}, {description, "foobar"}]))).

event_to_proplist_test() ->
  ?assertEqual([{service, "katja"}, {metric, 1}], event_to_proplist(#riemannpb_event{service="katja", metric_f=1.0, metric_sint64=1})),
  ?assertEqual([{service, "katja"}, {metric, 1.0}], event_to_proplist(#riemannpb_event{service="katja", metric_f=1.0, metric_d=1.0})),
  ?assertEqual([{state, "online"}, {service, "katja"}, {metric, 1}], event_to_proplist(#riemannpb_event{service="katja", state="online", metric_f=1.0, metric_sint64=1})),
  ?assertEqual([{service, "katja"}, {attributes, [{"foo", "bar"}]}],
               event_to_proplist(#riemannpb_event{service="katja", attributes=[#riemannpb_attribute{key="foo", value="bar"}]})).
-endif.
