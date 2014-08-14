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
% @doc The `katja_writer' module is responsible for sending metrics to Riemann.

-module(katja_writer).
-behaviour(gen_server).

-include("katja_types.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(DEFAULT_DEFAULTS, []).
-define(COMMON_FIELDS, [time, state, service, host, description, tags, ttl]).

% API
-export([
  start_link/0,
  start_link/1,
  stop/1,
  send_event/3,
  send_event_async/4,
  send_state/3,
  send_state_async/4,
  send_entities/3,
  send_entities_async/4
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

% @doc Starts a writer server process.
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
  gen_server:start_link(?MODULE, [], []).

% @doc Starts a writer server process and registers it as `{@module}'.
-spec start_link(register) -> {ok, pid()} | ignore | {error, term()}.
start_link(register) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% @doc Stops a writer server process.
-spec stop(katja:process()) -> ok.
stop(Pid) ->
  gen_server:call(Pid, terminate).

% @doc Sends an event to Riemann.<br />
%      If you do not set the `time' field manually, it will default to the local system time.<br />
%      You can set default values for `host', `tags' and `ttl' using the `defaults' configuration option.<br /><br />
%      Converting `Data' to a format that can be serialized happens inside the process
%      calling this function.
-spec send_event(katja:process(), katja_connection:transport(), katja:event()) -> ok | {error, term()}.
send_event(Pid, Transport, Data) ->
  Event = create_event(Data),
  gen_server:call(Pid, {send_message, Transport, event, Event}).

% @doc Sends an event to Riemann asynchronously. `SampleRate' is used to randomly drop some events. Setting it to
%      `1.0' will send all the data, but might lead to overload.<br />
%      If sending the event fails, you will <strong>not</strong> be notified about it.<br />
%      See the {@link send_event/3} documentation for more details.
-spec send_event_async(katja:process(), katja_connection:transport(), katja:event(), katja:sample_rate()) -> ok.
send_event_async(Pid, Transport, Data, SampleRate) ->
  case should_send_data(SampleRate) of
    false -> ok;
    true ->
      Event = create_event(Data),
      gen_server:cast(Pid, {send_message, Transport, event, Event})
  end.

% @doc Sends a state to Riemann.<br />
%      If you do not set the `time' field manually, it will default to the local system time.<br />
%      You can set default values for `host', `tags' and `ttl' using the `defaults' configuration option.<br /><br />
%      Converting `Data' to a format that can be serialized happens inside the process
%      calling this function.
-spec send_state(katja:process(), katja_connection:transport(), katja:event()) -> ok | {error, term()}.
send_state(Pid, Transport, Data) ->
  State = create_state(Data),
  gen_server:call(Pid, {send_message, Transport, state, State}).

% @doc Sends a state to Riemann asynchronously. `SampleRate' is used to randomly drop some states. Setting it to
%      `1.0' will send all the data, but might lead to overload.<br />
%      If sending the state fails, you will <strong>not</strong> be notified about it.<br />
%      See the {@link send_state/3} documentation for more details.
-spec send_state_async(katja:process(), katja_connection:transport(), katja:event(), katja:sample_rate()) -> ok.
send_state_async(Pid, Transport, Data, SampleRate) ->
  case should_send_data(SampleRate) of
    false -> ok;
    true ->
      State = create_state(Data),
      gen_server:cast(Pid, {send_message, Transport, state, State})
  end.

% @doc Sends multiple entities (events and/or states) to Riemann.<br />
%      If you do not set the `time' field manually, it will default to the local system time.<br />
%      You can set default values for `host', `tags' and `ttl' using the `defaults' configuration option.<br /><br />
%      Converting the `states' and `events' (inside the `Data' parameter) to a format that
%      can be serialized happens inside the process calling this function.
-spec send_entities(katja:process(), katja_connection:transport(), katja:entities()) -> ok | {error, term()}.
send_entities(Pid, Transport, Data) ->
  {EventEntities, StateEntities} = create_events_and_states(Data),
  Entities = StateEntities ++ EventEntities,
  gen_server:call(Pid, {send_message, Transport, entities, Entities}).

% @doc Sends multiple entities (events and/or states) to Riemann asynchronously. `SampleRate' is used to randomly drop some entities. Setting it to
%      `1.0' will send all the data, but might lead to overload.<br />
%      If sending the entities fails, you will <strong>not</strong> be notified about it.<br />
%      See the {@link send_entities/3} documentation for more details.
-spec send_entities_async(katja:process(), katja_connection:transport(), katja:entities(), katja:sample_rate()) -> ok.
send_entities_async(Pid, Transport, Data, SampleRate) ->
  case should_send_data(SampleRate) of
    false -> ok;
    true ->
      {EventEntities, StateEntities} = create_events_and_states(Data),
      Entities = StateEntities ++ EventEntities,
      gen_server:cast(Pid, {send_message, Transport, entities, Entities})
  end.

% gen_server

% @hidden
init([]) ->
  {ok, State} = katja_connection:connect(),
  {ok, State}.

% @hidden
handle_call({send_message, Transport, _Type, Data}, _From, State) ->
  Msg = create_message(Data),
  {Reply, State2} = send_message(Transport, Msg, State),
  {reply, Reply, State2};
handle_call(terminate, _From, State) ->
  {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

% @hidden
handle_cast({send_message, Transport, _Type, Data}, State) ->
  Msg = create_message(Data),
  {_Reply, State2} = send_message(Transport, Msg, State),
  {noreply, State2};
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

-spec should_send_data(katja:sample_rate()) -> boolean().
should_send_data(1.0) ->
  true;
should_send_data(SampleRate) ->
  ok = maybe_seed(),
  random:uniform() =< SampleRate.

maybe_seed() ->
  _ = case erlang:get(random_seed) of
    undefined -> random:seed(os:timestamp());
    {R, R, R} -> random:seed(os:timestamp());
    _ -> ok
  end,
  ok.

-spec create_event(katja:event()) -> riemannpb_event().
create_event(Data) ->
  Event = #riemannpb_event{},
  lists:foldl(fun(K, E) ->
    case lists:keyfind(K, 1, Data) of
      {K, V} -> set_event_field(K, V, E);
      false -> set_event_field(K, undefined, E)
    end
  end, Event, [attributes, metric|?COMMON_FIELDS]).

-spec create_state(katja:state()) -> riemannpb_state().
create_state(Data) ->
  State = #riemannpb_state{},
  lists:foldl(fun(K, E) ->
    case lists:keyfind(K, 1, Data) of
      {K, V} -> set_state_field(K, V, E);
      false -> set_state_field(K, undefined, E)
    end
  end, State, [once|?COMMON_FIELDS]).

-spec create_events_and_states(katja:entities()) -> {[riemannpb_event()], [riemannpb_state()]}.
create_events_and_states(Data) ->
  EventEntities = case lists:keyfind(events, 1, Data) of
    {events, Events} -> [create_event(E) || E <- Events];
    false -> []
  end,
  StateEntities = case lists:keyfind(states, 1, Data) of
    {states, States} -> [create_state(S) || S <- States];
    false -> []
  end,
  {EventEntities, StateEntities}.

-spec set_event_field(atom(), term(), riemannpb_event()) -> riemannpb_event().
set_event_field(time, undefined, E) -> E#riemannpb_event{time=current_timestamp()};
set_event_field(time, riemann, E) -> E#riemannpb_event{time=undefined};
set_event_field(time, V, E) -> E#riemannpb_event{time=V};
set_event_field(state, V, E) -> E#riemannpb_event{state=V};
set_event_field(service, V, E) -> E#riemannpb_event{service=V};
set_event_field(host, undefined, E) -> E#riemannpb_event{host=default_hostname()};
set_event_field(host, V, E) -> E#riemannpb_event{host=V};
set_event_field(description, V, E) -> E#riemannpb_event{description=V};
set_event_field(tags, undefined, E) -> E#riemannpb_event{tags=default_tags()};
set_event_field(tags, V, E) -> E#riemannpb_event{tags=V};
set_event_field(ttl, undefined, E) -> E#riemannpb_event{ttl=default_ttl()};
set_event_field(ttl, V, E) -> E#riemannpb_event{ttl=V};
set_event_field(attributes, undefined, E) -> E#riemannpb_event{attributes=[]};
set_event_field(attributes, V, E) ->
  Attrs = [#riemannpb_attribute{key=AKey, value=AVal} || {AKey, AVal} <- V],
  E#riemannpb_event{attributes=Attrs};
set_event_field(metric, undefined, E) -> E#riemannpb_event{metric_f = 0.0, metric_sint64 = 0};
set_event_field(metric, V, E) when is_integer(V) -> E#riemannpb_event{metric_f = V * 1.0, metric_sint64 = V};
set_event_field(metric, V, E) -> E#riemannpb_event{metric_f = V, metric_d = V}.

-spec set_state_field(atom(), term(), riemannpb_state()) -> riemannpb_state().
set_state_field(time, undefined, S) -> S#riemannpb_state{time=current_timestamp()};
set_state_field(time, riemann, S) -> S#riemannpb_state{time=undefined};
set_state_field(time, V, S) -> S#riemannpb_state{time=V};
set_state_field(state, V, S) -> S#riemannpb_state{state=V};
set_state_field(service, V, S) -> S#riemannpb_state{service=V};
set_state_field(host, undefined, S) -> S#riemannpb_state{host=default_hostname()};
set_state_field(host, V, S) -> S#riemannpb_state{host=V};
set_state_field(description, V, S) -> S#riemannpb_state{description=V};
set_state_field(tags, undefined, S) -> S#riemannpb_state{tags=default_tags()};
set_state_field(tags, V, S) -> S#riemannpb_state{tags=V};
set_state_field(ttl, undefined, S) -> S#riemannpb_state{ttl=default_ttl()};
set_state_field(ttl, V, S) -> S#riemannpb_state{ttl=V};
set_state_field(once, V, S) -> S#riemannpb_state{once=V}.

-spec default_hostname() -> iolist().
default_hostname() ->
  Defaults = application:get_env(katja, defaults, ?DEFAULT_DEFAULTS),
  case lists:keyfind(host, 1, Defaults) of
    {host, Host} -> Host;
    false ->
      {ok, Host} = inet:gethostname(),
      Host
  end.

-spec default_tags() -> [iolist()].
default_tags() ->
  Defaults = application:get_env(katja, defaults, ?DEFAULT_DEFAULTS),
  case lists:keyfind(tags, 1, Defaults) of
    {tags, Tags} -> Tags;
    false -> []
  end.

-spec default_ttl() -> float() | undefined.
default_ttl() ->
  Defaults = application:get_env(katja, defaults, ?DEFAULT_DEFAULTS),
  case lists:keyfind(ttl, 1, Defaults) of
    {ttl, TTL} -> TTL;
    false -> undefined
  end.

-spec current_timestamp() -> pos_integer().
current_timestamp() ->
  {MegaSecs, Secs, _MicroSecs} = os:timestamp(),
  MegaSecs * 1000000 + Secs.

-spec create_message(riemannpb_entity() | [riemannpb_entity()]) -> riemannpb_message().
create_message(Entity) when not is_list(Entity) ->
  create_message([Entity]);
create_message(Entities) ->
  {Events, States} = lists:splitwith(fun(Entity) -> is_record(Entity, riemannpb_event) end, Entities),
  #riemannpb_msg{events=Events, states=States}.

-spec send_message(katja_connection:transport(), riemannpb_message(), katja_connection:state()) -> {ok, katja_connection:state()} |
                                                                                                   {{error, term()}, katja_connection:state()}.
send_message(Transport, Msg, State) ->
  Msg2 = katja_pb:encode_riemannpb_msg(Msg),
  BinMsg = iolist_to_binary(Msg2),
  case katja_connection:send_message(Transport, BinMsg, State) of
    {{ok, _RetMsg}, State2} -> {ok, State2};
    {{error, _Reason}, _State2}=E -> E
  end.

% Tests (private functions)

-ifdef(TEST).
-define(TEST_DATA, [
  {time, 1},
  {state, "online"},
  {service, "katja"},
  {host, "localhost"},
  {description, "katja test"},
  {tags, ["foo", "bar"]}
]).

create_event_test() ->
  DefaultHost = default_hostname(),
  ?assertMatch(#riemannpb_event{time=1, state="online", service="katja", host="localhost", description="katja test", tags=["foo", "bar"]}, create_event(?TEST_DATA)),
  ?assertMatch(#riemannpb_event{time=1, state="online", service="katja", host="localhost", description="katja test", tags=[]},
               create_event(lists:keydelete(tags, 1, ?TEST_DATA))),
  ?assertMatch(#riemannpb_event{time=1, state="online", service="katja", host=DefaultHost, description="katja test", tags=["foo", "bar"]},
               create_event(lists:keydelete(host, 1, ?TEST_DATA))),
  ?assertMatch(#riemannpb_event{metric_f=0.0, metric_sint64=0}, create_event(?TEST_DATA)),
  ?assertMatch(#riemannpb_event{metric_f=1.0, metric_sint64=1}, create_event(?TEST_DATA ++ [{metric, 1}])),
  ?assertMatch(#riemannpb_event{metric_f=2.0, metric_d=2.0}, create_event(?TEST_DATA ++ [{metric, 2.0}])),
  ?assertMatch(#riemannpb_event{ttl=900.1, attributes=[#riemannpb_attribute{key="foo", value="bar"}]},
               create_event(?TEST_DATA ++ [{ttl, 900.1}, {attributes, [{"foo", "bar"}]}])),
  ?assertMatch(#riemannpb_event{time=undefined}, create_event([{time, riemann}])).

create_state_test() ->
  DefaultHost = default_hostname(),
  ?assertMatch(#riemannpb_state{time=1, state="online", service="katja", host="localhost", description="katja test", tags=["foo", "bar"]}, create_state(?TEST_DATA)),
  ?assertMatch(#riemannpb_state{time=1, state="online", service="katja", host="localhost", description="katja test", tags=[]},
               create_state(lists:keydelete(tags, 1, ?TEST_DATA))),
  ?assertMatch(#riemannpb_state{time=1, state="online", service="katja", host=DefaultHost, description="katja test", tags=["foo", "bar"]},
               create_state(lists:keydelete(host, 1, ?TEST_DATA))),
  ?assertMatch(#riemannpb_state{time=undefined}, create_state([{time, riemann}])).

create_message_test() ->
  Event = create_event(?TEST_DATA),
  State = create_state(?TEST_DATA),
  ?assertMatch(#riemannpb_msg{events=[#riemannpb_event{service="katja", host="localhost", description="katja test"}]}, create_message(Event)),
  ?assertMatch(#riemannpb_msg{events=[#riemannpb_event{service="katja", host="localhost", description="katja test"}]}, create_message([Event])),
  ?assertMatch(#riemannpb_msg{states=[#riemannpb_state{service="katja", host="localhost", description="katja test"}]}, create_message(State)),
  ?assertMatch(#riemannpb_msg{states=[#riemannpb_state{service="katja", host="localhost", description="katja test"}]}, create_message([State])),
  ?assertMatch(#riemannpb_msg{
                 events=[#riemannpb_event{service="katja", host="localhost", description="katja test"}],
                 states=[#riemannpb_state{service="katja", host="localhost", description="katja test"}]
               }, create_message([Event, State])).

default_hostname_test() ->
  Host = "an.host",
  ok = application:set_env(katja, defaults, [{host, Host}]),
  ?assertEqual(Host, default_hostname()),
  ok = application:unset_env(katja, defaults),
  ?assertNotEqual(Host, default_hostname()).

default_tags_test() ->
  Tags = ["some", "tags"],
  ok = application:set_env(katja, defaults, [{tags, Tags}]),
  ?assertEqual(Tags, default_tags()),
  ok = application:unset_env(katja, defaults),
  ?assertEqual([], default_tags()).

default_ttl_test() ->
  TTL = 60.0,
  ok = application:set_env(katja, defaults, [{ttl, TTL}]),
  ?assertEqual(TTL, default_ttl()),
  ok = application:unset_env(katja, defaults),
  ?assertEqual(undefined, default_ttl()).
-endif.
