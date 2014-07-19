# Katja

A simple [Riemann](http://riemann.io) client written in Erlang.

[![Build Status](https://travis-ci.org/nifoc/katja.png)](https://travis-ci.org/nifoc/katja)

## Status

This is alpha software. Things might still change in ways that break everything.

## Configuration

```erlang
% Defaults
[
  {katja, [
    {host, "127.0.0.1"},
    {port, 5555},
    {transport, detect},
    {pool, []},
    {defaults, []}
  ]}
].
```

**host**: Host Riemann is running on

**port**: Port Riemann is listening on

**transport**: The message transport that should be used (supported: `detect`, `udp`, `tcp`)

**pool**: List of processes that should not be started (and supervised) by Katja

**defaults**: Property list with default values for events and states (supported: `host`, `tags`, `ttl`)

## Examples

### Sending an event

```erlang
Event = [{service, "katja demo"}, {metric, 9000.1}],
ok = katja:send_event(Event).
```

An event in Katja is just a property list. A list of all possible properties can be found in the `katja` module.

### Sending a state

```erlang
State = [{service, "katja demo"}, {state, "testing"}],
ok = katja:send_state(State).
```

Just like events, a state is a property list. The properties can once again be found in the `katja` module.

### Sending multiple entities

```erlang
Event = [{service, "katja demo"}, {metric, 9000.1}],
State = [{service, "katja demo"}, {state, "testing"}],
ok = katja:send_entities([{events, [Event, Event]}, {states, [State, State]}]).
```

`katja:send_entities/1` takes a property list with two possible properties: `events` should be set to a list of events and `states` should be set to a list of states.

Both properties are optional, so `katja:send_entities/1` can be used to only send multiple events or states.

```erlang
Event = [{service, "katja demo"}, {metric, 9000.1}],
State = [{service, "katja demo"}, {state, "testing"}],
ok = katja:send_events([Event]),
ok = katja:send_states([State]).
```

`katja:send_events/1` and `katja:send_states/1` are also available to send multiple events or states. Both of them delegate to `katja:send_entities/1` internally.

### Querying

```erlang
{ok, Events} = katja:query("service = \"katja demo\"").
```

A query returns a list of events. Events are in the format that you specify when sending data to Riemann.

Another way to query Riemann is by using a `katja:event()` and the `katja:query_event/1` method.

```erlang
{ok, Events} = katja:query_event([{service, "katja demo"}]).
```

Katja will convert the event to a query string and query Riemann based on the generated string.

### Pooling

All the methods mentioned above optionally take a `katja:process()` as their first argument, enabling Katja to easily work with existing process pool implementations. `katja:process()` is either a `pid()` or one of the two following atoms: `katja_writer`, `katja_reader`.

The `atom()` cases should not be used directly, since `katja:send_event/1`, `katja:send_state/1`, `katja:query/1` etc. default to setting the correct atom value.

Additionally you can also "turn off" the `katja_writer` and `katja_reader` processes that are automatically started and supervised by adding their names to the `pool` configuration.

### Forcing a transport

You can force a message to be send via TCP or UDP. By default, the transport is chosen based on the size of a message: UDP is used for messages up to 16Kb in size, everything larger than that uses TCP. Querying Riemann always uses TCP.

```erlang
Event = [{service, "katja demo"}, {metric, 9000.1}],
ok = katja:send_event(katja_writer, tcp, Event).
```

The first argument to `katja:send_event/3`, `katja:send_events/3`, `katja:send_state/3`, `katja:send_states/3` and `katja:send_entities/3` is a `katja:process()`. If you're using one of these methods and don't use a process pool, it has to be set to `katja_writer`.

If (for example) you always want to send messages via TCP, you can set the `transport` configuration option to `tcp`.

## Resources

* [Generated EDoc](http://katja.nifoc.pw/0.4.2/) ([All Versions](http://katja.nifoc.pw))
* [Katja: Riemann Client Written In Erlang](https://blog.kempkens.io/posts/katja-riemann-client-written-in-erlang/)

## Related Projects

* [Katja VM Stats](https://github.com/nifoc/katja_vmstats) - Easily send information about the Erlang VM to Riemann

## License

[ISC](https://en.wikipedia.org/wiki/ISC_license).

```
Copyright (c) 2014, Daniel Kempkens <daniel@kempkens.io>

Permission to use, copy, modify, and/or distribute this software for any purpose
with or without fee is hereby granted, provided that the above copyright notice
and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF
THIS SOFTWARE.
```
