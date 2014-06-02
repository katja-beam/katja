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
% @hidden
% @author Daniel Kempkens <daniel@kempkens.io>
% @copyright {@years} Daniel Kempkens
% @version {@version}
% @doc Supervisor of the Katja application.

-module(katja_sup).
-behaviour(supervisor).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(DEFAULT_POOL, []).

% API
-export([start_link/0]).

% supervisor
-export([init/1]).

% API

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

% supervisor

init([]) ->
  Pool = application:get_env(katja, pool, ?DEFAULT_POOL),
  Children = maybe_add_child(katja_metrics, Pool, {katja_metrics,
    {katja_metrics, start_link, [register]},
    permanent,
    5000,
    worker,
    [katja_metrics]
  }, []),
  Children2 = maybe_add_child(katja_queries, Pool, {katja_queries,
    {katja_queries, start_link, [register]},
    permanent,
    5000,
    worker,
    [katja_queries]
  }, Children),
  {ok, {{one_for_one, 5, 10}, Children2}}.

% Private

-spec maybe_add_child(atom(), [atom()], tuple(), [tuple()]) -> [tuple()].
maybe_add_child(Child, Pool, Spec, Children) ->
  case lists:member(Child, Pool) of
    true -> Children;
    false -> [Spec | Children]
  end.

% Tests (private functions)

-ifdef(TEST).
maybe_add_child_test() ->
  ?assertEqual([{foo, bar}], maybe_add_child(test_one, [], {foo, bar}, [])),
  ?assertEqual([], maybe_add_child(test_two, [test_two], {foo, bar}, [])).
-endif.
