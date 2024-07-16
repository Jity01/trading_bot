---
title: ETC OCaml Starter Bot
---

# Overview

This code provides a basic starter ETC bot in OCaml.

The entry point is in [src/etc_bot_implementation.ml](./src/etc_bot_implementation.ml).
This file provides a basic example of how to use the
[Etc_exchange_driver](./exchange-driver) library to connect to, read
messages from, and send messages to an ETC exchange.

Note that you should feel free to change this code however you wish while
writing your ETC bot! Add, change, move, and delete code and files with aplomb!

Note: You may encounter usages of the
[`Async`](https://dev.realworldocaml.org/concurrent-programming.html) library. This
library is used for managing concurrent programming in OCaml. While it's mostly abstracted
away in the ETC scaffolding code, you might see the `Deferred.t` type pop up in some
places. You should feel free to grab a TA if you run into any issues that you don't
understand, or if you want to learn more about what's going on with `Async`.


# How to run

1. Update the `team_name` value in the [Exchange_driver
constants](./exchange-driver/constants.ml) to reflect your team name.

2. Build and run the starter bot code from the root directory of this code (the
same directory that this README lives in):
```
dune exec ./bin/main.exe -- -connect-to-test empty
```
You can also try to connect to the different test exchanges to see how they behave:
```
dune exec ./bin/main.exe -- -connect-to-test slower
```
```
dune exec ./bin/main.exe -- -connect-to-test prod-like
```

# Running in production

When you're ready to connect to the production exchange, you can run:

```
dune exec ./bin/main.exe -- -connect-to-prod
```

However, for running in production you might consider copying your compiled
binary elsewhere, rather than building and running anew each time. This will
allow you to continue developing your bot without worrying about how your
changes will affect your bot in production.

One way to do this:

```
cp _build/default/bin/main.exe ~/production_bot.exe
~/production_bot.exe -connect-to-prod
```

(Note: Please refer to the ETC handout about how to run your bot in a loop so
that it restarts when rounds end so you don't have to worry about restarting
your bot every time.)
