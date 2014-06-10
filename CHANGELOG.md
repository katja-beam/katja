# Changelog

## 0.5

* Rename `katja_metrics` to `katja_writer` and `katja_queries` to `katja_reader`

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
