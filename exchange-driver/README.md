---
title: ETC Exchange Driver
---

# Overview

This library provides an OCaml implementation of an ETC exchange driver. It
allows clients to connect, read from, and send messages to ETC exchanges using
the JSON exchange protocol.

# How to use

To use this library, you must update the `team_name` value in constants.ml to
reflect your team name.

The entry point to the library is the [Exchange_driver](./exchange_driver.mli) module. Call
`connect_and_run exchange_type ~f` to initiate a connection to the exchange.

`add_order`, `convert`, and `cancel` can be used with the `Exchange_driver.t`
given to `f` to send messages to the exchange.

# Code

[Exchange_driver](./exchange_driver.mli) defines how clients can initiate connections to the exchange.

[Exchange_type](./exchange_type.mli) defines the different type of exchanges that the client can
connect to.

[Constants](./constants.mli) defines static values used by the exchange driver.

[Client_message](./client_message.mli) defines the type of the messages that the client can send to
the exchange.

[Exchange_message](./exchange_message.mli) defines the type of the messages that the exchange can send
to the client.

[Types](./types.mli) offers value-restricted types that are used in `Client_message` and
`Exchange_message`, and can be useful to use when writing code for trading on
the exchange.
