---
title: Curiosity
---

# Application state

In a traditional [three-tier
architecture](https://en.wikipedia.org/wiki/Multitier_architecture)
application, the data tier, which stores the complete state of the system, is a
relational database, such as PostgreSQL.

In Curiosity, the complete state of the application is maintained in-memory
using [Software transactional
memory](https://en.wikipedia.org/wiki/Software_transactional_memory).

From the web interface, the complete application state can be browsed at
[`/state`](/state) (displayed using the internal state representation) and
[`/state.json`](/state.json) (displayed as JSON).

From the command-line, it is available with the `cty state` command, and from
the REPL, it is available with `state`.

Such complete representations are currently only practical for little data.

# Application state management

While Curiosity maintains only the state in-memory during nomal operations, it
is possible to write the state to disk or read the state from disk.

By default, the initial state of `cty repl` and `cty serve` are empty, and
their state is simply discarded when they shut down. It is possible to instruct
them to operate against a file: the initial state is read from the file, and
the final state is written back to the file upon exit. It is also possible for
`cty repl` to instruct it to talk to a running server.

Other `cty` commands on the other hand either "talk" to a state backed by a
file, or backed by a running server. By default, they operate against a file
called "state.json". When those commands are called through SSH, the machine is
configured so that they operate against the running server.

Note: the reason for differences in the default behavior of `cty repl` and `cty
server` vs. the other command, is to simplify calling `cty <some command>`
repeatedly from a shell, while `cty repl` and `cty serve` are typed less often
on the keyboard.

For commands that expect a state file, whether it is their default behavior or
they are instructed to do so, the state file must exit. The command to create a
state file is `cty init`.

Note: the reason to create explicitely a state file is to avoid modifying a
state file by mistake, e.g. from a script calling multiple `cty` commands.
