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
% @doc The `katja_metrics' module is responsible for sending metrics to Riemann.

-module(katja_metrics).
-behaviour(gen_server).

-include("katja_types.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(COMMON_FIELDS, [time, state, service, host, description, tags, ttl]).

% API
-export([
  start_link/0,
  start_link/1,
  stop/1,
  send_event/2,
  send_state/2,
  send_entities/2
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

% @doc Starts a metrics server process.
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
  gen_server:start_link(?MODULE, [], []).

% @doc Starts a metrics server process and registers it as `{@module}'.
-spec start_link(register) -> {ok, pid()} | ignore | {error, term()}.
start_link(register) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% @doc Stops a metrics server process.
-spec stop(katja:process()) -> ok.
stop(Pid) ->
  gen_server:call(Pid, terminate).

% @doc Sends an event to Riemann.<br />
%      Converting `Data' to a format that can be serialized happens inside the process
%      calling this function.
-spec send_event(katja:process(), katja:event()) -> ok | {error, term()}.
send_event(Pid, Data) ->
  Event = create_event(Data),
  gen_server:call(Pid, {send_message, event, Event}).

% @doc Sends a state to Riemann.<br />
%      Converting `Data' to a format that can be serialized happens inside the process
%      calling this function.
-spec send_state(katja:process(), katja:event()) -> ok | {error, term()}.
send_state(Pid, Data) ->
  State = create_state(Data),
  gen_server:call(Pid, {send_message, state, State}).

% @doc Sends multiple entities (events and/or states) to Riemann.<br />
%      Converting the `states' and `events' (inside the `Data' parameter) to a format that
%      can be serialized happens inside the process calling this function.
-spec send_entities(katja:process(), katja:entities()) -> ok | {error, term()}.
send_entities(Pid, Data) ->
  StateEntities = case lists:keyfind(states, 1, Data) of
    {states, States} -> [create_state(S) || S <- States];
    false -> []
  end,
  EventEntities = case lists:keyfind(events, 1, Data) of
    {events, Events} -> [create_event(E) || E <- Events];
    false -> []
  end,
  Entities = StateEntities ++ EventEntities,
  gen_server:call(Pid, {send_message, entities, Entities}).

% gen_server

% @hidden
init([]) ->
  {ok, State} = katja_connection:connect(),
  {ok, State}.

% @hidden
handle_call({send_message, _Type, Data}, _From, State) ->
  Msg = create_message(Data),
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

-spec create_event(katja:event()) -> riemannpb_event().
create_event(Data) ->
  Event = #riemannpb_event{},
  lists:foldr(fun(K, E) ->
    case lists:keyfind(K, 1, Data) of
      {K, V} -> set_event_field(K, V, E);
      false -> set_event_field(K, undefined, E)
    end
  end, Event, [attributes, metric|?COMMON_FIELDS]).

-spec create_state(katja:state()) -> riemannpb_state().
create_state(Data) ->
  State = #riemannpb_state{},
  lists:foldr(fun(K, E) ->
    case lists:keyfind(K, 1, Data) of
      {K, V} -> set_state_field(K, V, E);
      false -> set_state_field(K, undefined, E)
    end
  end, State, [once|?COMMON_FIELDS]).

-spec set_event_field(atom(), term(), riemannpb_event()) -> riemannpb_event().
set_event_field(time, undefined, E) -> E#riemannpb_event{time=current_timestamp()};
set_event_field(time, V, E) -> E#riemannpb_event{time=V};
set_event_field(state, V, E) -> E#riemannpb_event{state=V};
set_event_field(service, V, E) -> E#riemannpb_event{service=V};
set_event_field(host, undefined, E) -> E#riemannpb_event{host=default_hostname()};
set_event_field(host, V, E) -> E#riemannpb_event{host=V};
set_event_field(description, V, E) -> E#riemannpb_event{description=V};
set_event_field(tags, undefined, E) -> E#riemannpb_event{tags=[]};
set_event_field(tags, V, E) -> E#riemannpb_event{tags=V};
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
set_state_field(time, V, S) -> S#riemannpb_state{time=V};
set_state_field(state, V, S) -> S#riemannpb_state{state=V};
set_state_field(service, V, S) -> S#riemannpb_state{service=V};
set_state_field(host, undefined, S) -> S#riemannpb_state{host=default_hostname()};
set_state_field(host, V, S) -> S#riemannpb_state{host=V};
set_state_field(description, V, S) -> S#riemannpb_state{description=V};
set_state_field(tags, undefined, S) -> S#riemannpb_state{tags=[]};
set_state_field(tags, V, S) -> S#riemannpb_state{tags=V};
set_state_field(ttl, V, S) -> S#riemannpb_state{ttl=V};
set_state_field(once, V, S) -> S#riemannpb_state{once=V}.

-spec default_hostname() -> string().
default_hostname() ->
  {ok, Host} = inet:gethostname(),
  Host.

-spec current_timestamp() -> pos_integer().
current_timestamp() ->
  {MegaSecs, Secs, _MicroSecs} = os:timestamp(),
  MegaSecs * 1000000 + Secs.

-spec create_message(riemannpb_entity() | [riemannpb_entity()]) -> riemannpb_message().
create_message(Entity) when not is_list(Entity) ->
  create_message([Entity]);
create_message(Entities) ->
  {Events, States} = lists:splitwith(fun(Entity) ->
    if
      is_record(Entity, riemannpb_event) -> true;
      true -> false
    end
  end, Entities),
  #riemannpb_msg{events=Events, states=States}.

-spec send_message(riemannpb_message(), katja_connection:state()) -> {ok, katja_connection:state()} | {{error, term()}, katja_connection:state()}.
send_message(Msg, State) ->
  Msg2 = katja_pb:encode_riemannpb_msg(Msg),
  BinMsg = iolist_to_binary(Msg2),
  case katja_connection:send_message(BinMsg, State) of
    {{ok, _RetMsg}, State2} -> {ok, State2};
    {{error, Reason}, State2} -> {{error, Reason}, State2}
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
               create_event(?TEST_DATA ++ [{ttl, 900.1}, {attributes, [{"foo", "bar"}]}])).

create_state_test() ->
  DefaultHost = default_hostname(),
  ?assertMatch(#riemannpb_state{time=1, state="online", service="katja", host="localhost", description="katja test", tags=["foo", "bar"]}, create_state(?TEST_DATA)),
  ?assertMatch(#riemannpb_state{time=1, state="online", service="katja", host="localhost", description="katja test", tags=[]},
               create_state(lists:keydelete(tags, 1, ?TEST_DATA))),
  ?assertMatch(#riemannpb_state{time=1, state="online", service="katja", host=DefaultHost, description="katja test", tags=["foo", "bar"]},
               create_state(lists:keydelete(host, 1, ?TEST_DATA))).

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
-endif.
