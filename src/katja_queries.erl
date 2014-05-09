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
% @doc The `katja_queries' module is responsible for querying Riemann.<br />
%      Riemann currently does not provide real documentation on how to write queries, but there are a couple of resources
%      that can help you get started with writing them:
%      <ul>
%        <li><a href="https://github.com/aphyr/riemann/blob/master/src/riemann/Query.g">Grammar</a></li>
%        <li><a href="https://github.com/aphyr/riemann/blob/master/test/riemann/query_test.clj">Test Suite</a></li>
%      </ul>

-module(katja_queries).
-behaviour(gen_server).

-include("katja_types.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

% API
-export([
  start_link/0,
  query/1
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

% @doc Starts the querying server process.
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% @doc Sends a query string to Riemann and returns a list of matching events.<br />
%      Example queries can be found in the
%      <a href="https://github.com/aphyr/riemann/blob/master/test/riemann/query_test.clj">Riemann test suite</a>.
-spec query(string()) -> {ok, [katja:event()]} | {error, term()}.
query(Query) ->
  Msg = create_query_message(Query),
  gen_server:call(?MODULE, {query, Msg}).

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

-spec create_query_message(string()) -> riemannpb_message().
create_query_message(Query) ->
  #riemannpb_msg{
     query=#riemannpb_query{string=Query}
  }.

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
event_to_proplist(Event) ->
  Event2 = lists:zip(record_info(fields, riemannpb_event), tl(tuple_to_list(Event))),
  Event3 = lists:filter(fun({_Key, Value}) -> Value =/= undefined andalso Value =/= [] end, Event2),
  Event4 = case Event#riemannpb_event.metric_sint64 of
    undefined -> [{metric, Event#riemannpb_event.metric_f}|Event3];
    Int -> [{metric, Int}|Event3]
  end,
  Event5 = case Event#riemannpb_event.attributes of
    [] -> Event4;
    Attrs ->
      Attrs2 = [{AKey, AVal} || {riemannpb_attribute, AKey, AVal} <- Attrs],
      lists:keyreplace(attributes, 1, Event4, {attributes, Attrs2})
  end,
  lists:foldr(fun(Key, Acc) -> lists:keydelete(Key, 1, Acc) end, Event5, [metric_sint64, metric_f, metric_d]).

% Tests (private functions)

-ifdef(TEST).
create_query_message_test() ->
  ?assertMatch(#riemannpb_msg{query=#riemannpb_query{string="foo"}}, create_query_message("foo")).

event_to_proplist_test() ->
  ?assertEqual([{metric, 1}, {service, "katja"}], event_to_proplist(#riemannpb_event{service="katja", metric_f=1.0, metric_sint64=1})),
  ?assertEqual([{metric, 1.0}, {service, "katja"}], event_to_proplist(#riemannpb_event{service="katja", metric_f=1.0, metric_d=1.0})).
-endif.
