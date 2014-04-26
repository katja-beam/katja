# Katja

A simple [Riemann](http://riemann.io) client written in Erlang.

## Status

This is alpha software. Things might still change in ways that break everything.

## Examples

### Sending an event

```erlang
Event = [{service, "katja demo"}, {metric, 9000.1}],
ok = katja:send_event(Event).
```

An event in Katja is just a property list. A list of all possible properties can be found in the `katja` module.

### Querying

```erlang
{ok, Events} = katja:query("service = \"katja demo\"").
```

A query returns a list of events. Events are in the format that you specify when sending data to Riemann.

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
