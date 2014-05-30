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
%      While it is possible to use `katja_metrics' and `katja_queries' directly, the recommended way is to use the functions defined
%      in this module instead.

-module(katja).

-include("katja_types.hrl").

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

-type process() :: pid() | atom().

-export_type([
  event/0,
  state/0,
  entities/0,
  process/0
]).

% API
-export([
  send_event/1,
  send_event/2,
  send_events/1,
  send_events/2,
  send_state/1,
  send_state/2,
  send_states/1,
  send_states/2,
  send_entities/1,
  send_entities/2,
  query/1,
  query/2,
  query_event/1,
  query_event/2
]).

% API

% @doc Delegates to {@link send_event/2}. `Pid' is set to `katja_metrics'.
-spec send_event(event()) -> ok | {error, term()}.
send_event(Data) ->
  send_event(katja_metrics, Data).

% @doc Sends a single event to Riemann. Delegates to {@link katja_metrics:send_event/1}.
-spec send_event(process(), event()) -> ok | {error, term()}.
send_event(Pid, Data) ->
  katja_metrics:send_event(Pid, Data).

% @doc Delegates to {@link send_events/2}. `Pid' is set to `katja_metrics'.
-spec send_events([event()]) -> ok | {error, term()}.
send_events(Data) ->
  send_events(katja_metrics, Data).

% @doc Sends multiple events to Riemann. Simple wrapper around {@link send_entities/1}.
-spec send_events(process(), [event()]) -> ok | {error, term()}.
send_events(Pid, Data) ->
  send_entities(Pid, [{events, Data}]).

% @doc Delegates to {@link send_state/2}. `Pid' is set to `katja_metrics'.
-spec send_state(state()) -> ok | {error, term()}.
send_state(Data) ->
  send_state(katja_metrics, Data).

% @doc Sends a single state to Riemann. Delegates to {@link katja_metrics:send_state/1}.
-spec send_state(process(), state()) -> ok | {error, term()}.
send_state(Pid, Data) ->
  katja_metrics:send_state(Pid, Data).

% @doc Delegates to {@link send_states/2}. `Pid' is set to `katja_metrics'.
-spec send_states([state()]) -> ok | {error, term()}.
send_states(Data) ->
  send_states(katja_metrics, Data).

% @doc Sends multiple states to Riemann. Simple wrapper around {@link send_entities/1}.
-spec send_states(process(), [state()]) -> ok | {error, term()}.
send_states(Pid, Data) ->
  send_entities(Pid, [{states, Data}]).

% @doc Delegates to {@link send_entities/2}. `Pid' is set to `katja_metrics'.
-spec send_entities(entities()) -> ok | {error, term()}.
send_entities(Data) ->
  send_entities(katja_metrics, Data).

% @doc Delegates to {@link katja_metrics:send_entities/1}.
-spec send_entities(process(), entities()) -> ok | {error, term()}.
send_entities(Pid, Data) ->
  katja_metrics:send_entities(Pid, Data).

% @doc Delegates to {@link query/2}. `Pid' is set to `katja_queries'.
-spec query(string()) -> {ok, [event()]} | {error, term()}.
query(Query) ->
  query(katja_queries, Query).

% @doc Delegates to {@link katja_queries:query/1}.
-spec query(process(), string()) -> {ok, [event()]} | {error, term()}.
query(Pid, Query) ->
  katja_queries:query(Pid, Query).

% @doc Delegates to {@link query_event/2}. `Pid' is set to `katja_queries'.
-spec query_event(event()) -> {ok, [event()]} | {error, term()}.
query_event(Event) ->
  query_event(katja_queries, Event).

% @doc Delegates to {@link katja_queries:query_event/1}.
-spec query_event(process(), event()) -> {ok, [event()]} | {error, term()}.
query_event(Pid, Event) ->
  katja_queries:query_event(Pid, Event).
