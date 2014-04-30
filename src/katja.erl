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
% @copyright 2014 Daniel Kempkens
% @version 1.0
% @doc This is the main module of the Katja application. It provides the public API.

-module(katja).

-include("katja_types.hrl").

-type riemann_time() :: {time, non_neg_integer()}.
-type riemann_state() :: {state, string()}.
-type riemann_service() :: {service, string()}.
-type riemann_host() :: {host, string()}.
-type riemann_description() :: {description, string()}.
-type riemann_tags() :: {tags, [string()]}.
-type riemann_ttl() :: {ttl, float()}.
-type riemann_attributes() :: {attributes, [{string(), string()}]}.
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

-export_type([
  event/0,
  state/0,
  entities/0
]).

% API
-export([
  send_event/1,
  send_events/1,
  send_state/1,
  send_states/1,
  send_entities/1,
  query/1
]).

% API

% @doc Delegates to {@link katja_metrics:send_event/1}.
-spec send_event(event()) -> ok | {error, term()}.
send_event(Data) ->
  katja_metrics:send_event(Data).

% @doc Sends multiple events to Riemann. Simple wrapper around {@link send_entities/1}.
-spec send_events([event()]) -> ok | {error, term()}.
send_events(Data) ->
  send_entities([{events, Data}]).

% @doc Delegates to {@link katja_metrics:send_state/1}.
-spec send_state(state()) -> ok | {error, term()}.
send_state(Data) ->
  katja_metrics:send_state(Data).

% @doc Sends multiple states to Riemann. Simple wrapper around {@link send_entities/1}.
-spec send_states([state()]) -> ok | {error, term()}.
send_states(Data) ->
  send_entities([{states, Data}]).

% @doc Delegates to {@link katja_metrics:send_entities/1}.
-spec send_entities(entities()) -> ok | {error, term()}.
send_entities(Data) ->
  katja_metrics:send_entities(Data).

% @doc Delegates to {@link katja_queries:query/1}.
-spec query(string()) -> {ok, [event()]} | {error, term()}.
query(Query) ->
  katja_queries:query(Query).
