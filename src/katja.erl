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
% @doc This is the main module of the Katja application: It provides the public API.<br />
%      While it is possible to use `katja_writer' and `katja_reader' directly, the recommended way is to use the functions defined
%      in this module instead.

-module(katja).

% Types

-type riemann_time() :: {time, non_neg_integer() | riemann}.
-type riemann_state() :: {state, iolist()}.
-type riemann_service() :: {service, iolist()}.
-type riemann_host() :: {host, iolist()}.
-type riemann_description() :: {description, iolist()}.
-type riemann_tags() :: {tags, [iolist()]}.
-type riemann_ttl() :: {ttl, float()}.
-type riemann_attributes() :: {attributes, [{iolist(), iolist()}]}.
-type riemann_metric() :: {metric, number()}.
-type riemann_once() :: {once, boolean()}.

-type riemann_event_opts() :: riemann_time() | riemann_state() | riemann_service() |
                              riemann_host() | riemann_description() | riemann_tags() |
                              riemann_ttl() | riemann_attributes() | riemann_metric().
-type riemann_state_opts() :: riemann_time() | riemann_state() | riemann_service() |
                              riemann_host() | riemann_description() | riemann_tags() |
                              riemann_ttl() | riemann_once().

-type event() :: [riemann_event_opts()].
-type state() :: [riemann_state_opts()].
-type entities() :: [{events, [event()]} | {states, [state()]}].

-type process() :: pid() | katja_writer | katja_reader.

-export_type([
  event/0,
  state/0,
  entities/0,
  process/0
]).

% API
-export([
  start/0,
  stop/0,
  send_event/1,
  send_event/2,
  send_event/3,
  send_events/1,
  send_events/2,
  send_events/3,
  send_state/1,
  send_state/2,
  send_state/3,
  send_states/1,
  send_states/2,
  send_states/3,
  send_entities/1,
  send_entities/2,
  send_entities/3,
  query/1,
  query/2,
  query_event/1,
  query_event/2
]).

% API

% @doc Starts the Katja application and all of its dependencies. This is really only meant for usage inside the console.
-spec start() -> ok.
start() ->
  ok = application:start(protobuffs),
  ok = application:start(katja),
  ok.

% @doc Stops the Katja application and all of its dependencies. This is really only meant for usage inside the console.
-spec stop() -> ok.
stop() ->
  ok = application:stop(katja),
  ok = application:stop(protobuffs),
  ok.

% @doc Delegates to {@link send_event/2}. `Pid' is set to `katja_writer'.
-spec send_event(event()) -> ok | {error, term()}.
send_event(Data) ->
  send_event(katja_writer, Data).

% @doc Delegates to {@link send_event/3}. `Transport' is set to `config'.
-spec send_event(process(), event()) -> ok | {error, term()}.
send_event(Pid, Data) ->
  send_event(Pid, config, Data).

% @doc Sends a single event to Riemann. Delegates to {@link katja_writer:send_event/3}.
-spec send_event(process(), katja_connection:transport(), event()) -> ok | {error, term()}.
send_event(Pid, Transport, Data) ->
  katja_writer:send_event(Pid, Transport, Data).

% @doc Delegates to {@link send_events/2}. `Pid' is set to `katja_writer'.
-spec send_events([event()]) -> ok | {error, term()}.
send_events(Data) ->
  send_events(katja_writer, Data).

% @doc Delegates to {@link send_events/3}. `Transport' is set to `config'.
-spec send_events(process(), [event()]) -> ok | {error, term()}.
send_events(Pid, Data) ->
  send_events(Pid, config, Data).

% @doc Sends multiple events to Riemann. Simple wrapper around {@link send_entities/3}.
-spec send_events(process(), katja_connection:transport(), [event()]) -> ok | {error, term()}.
send_events(Pid, Transport, Data) ->
  send_entities(Pid, Transport, [{events, Data}]).

% @doc Delegates to {@link send_state/2}. `Pid' is set to `katja_writer'.
-spec send_state(state()) -> ok | {error, term()}.
send_state(Data) ->
  send_state(katja_writer, Data).

% @doc Delegates to {@link send_state/3}. `Transport' is set to `config'.
-spec send_state(process(), state()) -> ok | {error, term()}.
send_state(Pid, Data) ->
  send_state(Pid, config, Data).

% @doc Sends a single state to Riemann. Delegates to {@link katja_writer:send_state/3}.
-spec send_state(process(), katja_connection:transport(), state()) -> ok | {error, term()}.
send_state(Pid, Transport, Data) ->
  katja_writer:send_state(Pid, Transport, Data).

% @doc Delegates to {@link send_states/2}. `Pid' is set to `katja_writer'.
-spec send_states([state()]) -> ok | {error, term()}.
send_states(Data) ->
  send_states(katja_writer, Data).

% @doc Delegates to {@link send_states/3}. `Transport' is set to `config'.
-spec send_states(process(), [state()]) -> ok | {error, term()}.
send_states(Pid, Data) ->
  send_states(Pid, config, Data).

% @doc Sends multiple states to Riemann. Simple wrapper around {@link send_entities/3}.
-spec send_states(process(), katja_connection:transport(), [state()]) -> ok | {error, term()}.
send_states(Pid, Transport, Data) ->
  send_entities(Pid, Transport, [{states, Data}]).

% @doc Delegates to {@link send_entities/2}. `Pid' is set to `katja_writer'.
-spec send_entities(entities()) -> ok | {error, term()}.
send_entities(Data) ->
  send_entities(katja_writer, Data).

% @doc Delegates to {@link send_entities/3}. `Transport' is set to `config'.
-spec send_entities(process(), entities()) -> ok | {error, term()}.
send_entities(Pid, Data) ->
  send_entities(Pid, config, Data).

% @doc Sends multiple entities (events and/or states) to Riemann. Delegates to {@link katja_writer:send_entities/3}.
-spec send_entities(process(), katja_connection:transport(), entities()) -> ok | {error, term()}.
send_entities(Pid, Transport, Data) ->
  katja_writer:send_entities(Pid, Transport, Data).

% @doc Delegates to {@link query/2}. `Pid' is set to `katja_reader'.
-spec query(string()) -> {ok, [event()]} | {error, term()}.
query(Query) ->
  query(katja_reader, Query).

% @doc Delegates to {@link katja_reader:query/2}.
-spec query(process(), string()) -> {ok, [event()]} | {error, term()}.
query(Pid, Query) ->
  katja_reader:query(Pid, Query).

% @doc Delegates to {@link query_event/2}. `Pid' is set to `katja_reader'.
-spec query_event(event()) -> {ok, [event()]} | {error, term()}.
query_event(Event) ->
  query_event(katja_reader, Event).

% @doc Delegates to {@link katja_reader:query_event/2}.
-spec query_event(process(), event()) -> {ok, [event()]} | {error, term()}.
query_event(Pid, Event) ->
  katja_reader:query_event(Pid, Event).
