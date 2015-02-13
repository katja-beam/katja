# Changelog

## 0.8

* The `hostname` field of an entity can now default to `node()` (`node_name`) or the name of VM (`vm_name`), where VM name is defined as `node()` with a `.` instead of an `@` (thanks [dch](https://github.com/dch))
* A tag of `instance` (atom) will tag entities with "instance: *XXX*", where *XXX* refers to the first part of the name of the VM (thanks [dch](https://github.com/dch))

## 0.7.1

[Documentation](http://katja.nifoc.pw/0.7.1/)

* Handle some common socket errors (thanks [@puzza007](https://github.com/puzza007))

## 0.7

[Documentation](http://katja.nifoc.pw/0.7/)

* Change the restart strategy of the supervisor from `one_for_one` to `one_for_all`
* Add `katja:send_event_async/4`, `katja:send_events_async/4`, `katja:send_state_async/4`, `katja:send_states_async/4` and `katja:send_entities_async/4` with a `SampleRate` parameter, allowing you to easily sample metrics

## 0.6

[Documentation](http://katja.nifoc.pw/0.6/)

* Switch from [Rebar](https://github.com/rebar/rebar) to [erlang.mk](https://github.com/ninenines/erlang.mk)
* Add `katja:send_event_async/{1,2,3}`, `katja:send_events_async/{1,2,3}`, `katja:send_state_async/{1,2,3}`, `katja:send_states_async/{1,2,3}` and `katja:send_entities_async/{1,2,3}` to asynchronously send data to Riemann
* Add `katja:query_async/{1,2}` and `katja:query_event_async/{1,2}` to query Riemann asynchronously
* Add `katja:stop/0` to stop the Katja application and all of its dependencies
* Fixed a potential infinite loop when sending invalid data via TCP

## 0.5

[Documentation](http://katja.nifoc.pw/0.5/)

* Rename `katja_metrics` to `katja_writer` and `katja_queries` to `katja_reader`
* `katja:send_event/3`, `katja:send_events/3`, `katja:send_state/3`, `katja:send_states/3` and `katja:send_entities/3` can be used to force a transport (supported: `detect`, `udp`, `tcp`)
* A default message transport (affecting only entities that are sent to Riemann) can be set using the `transport` configuration option
* Add `katja:start/0` to start the Katja application and all of its dependencies

## 0.4.2

[Documentation](http://katja.nifoc.pw/0.4.2/)

* Fix spelling mistake in `katja.app.src`

## 0.4.1

[Documentation](http://katja.nifoc.pw/0.4.1/)

* Make `protobuffs` a runtime dependency (thanks [@robashton](https://github.com/robashton))

## 0.4

[Documentation](http://katja.nifoc.pw/0.4/)

* The `time` field (of events and states) now defaults to the local system time
* "Forcing" Riemann to set the `time` field can be done by setting it to `riemann`
* A default `host`, default `tags` and `ttl` can be set using the new `defaults` configuration option

## 0.3

[Documentation](http://katja.nifoc.pw/0.3/)

* Pooling support ([Issue #1](https://github.com/nifoc/katja/issues/1))

## 0.2

[Documentation](http://katja.nifoc.pw/0.2/)

* Support for querying Riemann based on a `katja:event()`

## 0.1.1

[Documentation](http://katja.nifoc.pw/0.1.1/)

* Fixed getting default `host` and `port` values

## 0.1

* Initial release
