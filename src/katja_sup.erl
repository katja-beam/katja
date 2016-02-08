% Copyright (c) 2014-2016, Daniel Kempkens <daniel@kempkens.io>
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
  Children = maybe_add_child(katja_writer, Pool, child_spec(katja_writer), []),
  Children2 = maybe_add_child(katja_reader, Pool, child_spec(katja_reader), Children),
  {ok, {{one_for_all, 10, 10}, Children2}}.

% Private

-spec child_spec(atom()) -> supervisor:child_spec().
child_spec(Mod) ->
  {Mod,
    {Mod, start_link, [register]},
    permanent,
    5000,
    worker,
    [Mod]
  }.

-spec maybe_add_child(atom(), [atom()], supervisor:child_spec(), [supervisor:child_spec()]) -> [supervisor:child_spec()].
maybe_add_child(Child, Pool, Spec, Children) ->
  case lists:member(Child, Pool) of
    true -> Children;
    false -> [Spec | Children]
  end.

% Tests (private functions)

-ifdef(TEST).
maybe_add_child_test() ->
  SpecA = child_spec(foo),
  SpecB = child_spec(bar),
  ?assertEqual([SpecA], maybe_add_child(test_one, [], SpecA, [])),
  ?assertEqual([], maybe_add_child(test_two, [test_two], SpecB, [])).
-endif.
