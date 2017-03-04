![Transient logo](https://raw.githubusercontent.com/transient-haskell/transient/master/logo.png)
=========

[![Hackage](https://img.shields.io/hackage/v/transient-universe.svg)](http://hackage.haskell.org/package/transient-universe)
[![Stackage LTS](http://stackage.org/package/transient-universe/badge/lts)](http://stackage.org/lts/package/transient-universe)
[![Stackage Nightly](http://stackage.org/package/transient-universe/badge/nightly)](http://stackage.org/nightly/package/transient-universe)
[![Build Status](https://travis-ci.org/agocorona/transient-universe.png?branch=master)](https://travis-ci.org/agocorona/transient-universe)
[![Gitter](https://badges.gitter.im/theam/haskell-do.svg)](https://gitter.im/Transient-Transient-Universe-HPlay/Lobby?utm_source=share-link&utm_medium=link&utm_campaign=share-link)

See the [Wiki](https://github.com/agocorona/transient/wiki)

transient-universe is the distributed computing extension of [transient](https://github.com/agocorona/transient).  It support moving computations between Haskell closures in different computers in the network. Even among different architectures:  Linux nodes can work with windows and browser nodes running haskell compiled with [ghcjs](https://github.com/ghcjs/ghcjs).

The primitives that perform the moving of computations are called `wormhole` and `teleport`, the names express the semantics. Hence the name of the package.

All the nodes run the same program compiled for different architectures. It defines a Cloud computation (monad). It is a thin layer on top of transient with additional primitives and services that run a single program in one or many nodes.

Browser integration
==================

Browser nodes, running transient programs compiled with ghcjs are integrated with server nodes, using websockets for communication. Just compile the program with ghcjs and point the browser to http://server:port. The server nodes have a HTTP server that will send the compiled program to the browser.

Distributed Browser/server Widgets
-------
Browser nodes can integrate Hplayground for ghcjs, a reactive client side library based in trasient (package ghcjs-hplay) they can create widgets with HTML form elements and control the server nodes. A computation can move from browser to server and back at runtime despite the different architecture.

Widgets with code running in browser and servers can compose with other widgets. A Browser node can gain access to many server nodes trough the  server that delivered the web application. 

These features can make transient ideal for client as well as server side-driven applications, whenever distribution and push-driven reactivity is necessary either in the servers or in the browser clients.

New
===
The last release add 

  - Hooks for secure communications
  - Client websocket connections to connect with nodes within firewalled servers
  - No network traffic when a node invokes itself

Map-reduce
==========
transient-universe implements map-reduce in the style of [spark](http://spark.apache.org) as a particular case. It is at the same time a hard test of the distributed primitives since it involves a complex choreography of movement of computations. It supports in memory operations and caching. Resilience (restart from the last checkpoint in case of failure) is not implemented but it is foreseen.

Look at [this article](https://www.schoolofhaskell.com/user/agocorona/estimation-of-using-distributed-computing-streaming-transient-effects-vi-1#distributed-datasets)

There is a runnable example: [DistrbDataSets.hs](https://github.com/agocorona/transient-universe/blob/master/examples/DistrbDataSets.hs) that you can executed with:

> runghc ./examples/DistrbDataSets.hs

It uses a number of simulated nodes to calculate the frequency of words in a long text.

Services
========
Services communicate two different transient applications. This allows to divide the running application in different independent tiers.   No documentation is available yet. Sorry.

General distributed primitives
=============================
`teleport` is a  primitive that translates computations back and forth reusing an already opened connection.

The connection is initiated by `wormhole`  with another node. This can be done anywhere in a computation without breaking composability. As always, Everything is composable.

Both primitives support also streaming among nodes in an efficient way. It means that a remote call can return not just a single response, but many of them.

All the other distributed primitives: `runAt`, `streamFrom` `clustered` etc are rewritten in terms of these two.

How to run the ghcjs example:
=============================

See the  distributed examples in the [transient-examples](https://github.com/transient-haskell/transient) repository

See this [video](https://www.livecoding.tv/agocorona/videos/Ke1Qz-seamless-composable-web-programming) to see this example running:

The test program run among other things, two copies of a widget that start, stop and display a counter that run in the server.

Documentation
=============

The [Wiki](https://github.com/agocorona/transient/wiki) is more user oriented

My video sessions in [livecoding.tv](https://www.livecoding.tv/agocorona/videos/) not intended as tutorials or presentations, but show some of the latest features running.

The articles are more technical:

- [Philosophy, async, parallelism, thread control, events, Session state](https://www.fpcomplete.com/user/agocorona/EDSL-for-hard-working-IT-programmers?show=tutorials)
- [Backtracking and undoing IO transactions](https://www.fpcomplete.com/user/agocorona/the-hardworking-programmer-ii-practical-backtracking-to-undo-actions?show=tutorials)
- [Non-deterministic list like processing, multithreading](https://www.fpcomplete.com/user/agocorona/beautiful-parallel-non-determinism-transient-effects-iii?show=tutorials)
- [Distributed computing](https://www.fpcomplete.com/user/agocorona/moving-haskell-processes-between-nodes-transient-effects-iv?show=tutorials)
- [Publish-Subscribe variables](https://www.schoolofhaskell.com/user/agocorona/publish-subscribe-variables-transient-effects-v)
- [Distributed streaming, map-reduce](https://www.schoolofhaskell.com/user/agocorona/estimation-of-using-distributed-computing-streaming-transient-effects-vi-1)

These articles contain executable examples (not now, since the site no longer support the execution of Haskell snippets).



Future plans
============
The only way to improve it is using it. Please send me bugs and additional functionalities!

-I plan to improve map-reduce to create a viable platform for serious data analysis and machine learning using haskell. It will have a  web notebook running in the browser.

-Create services and examples for general Web applications with distributed servers and create services for them





