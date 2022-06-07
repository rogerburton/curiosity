# Curiosity - A prototype application for Smart

This repository contains a prototype application for the
[Smart](https://github.com/smartcoop/) cooperative. What we hear about
"prototype" is written in `start-servant`'s
[README](https://github.com/noteed/start-servant#readme).

# Related repositories

- The prototype uses [`design-hs`](https://github.com/smartcoop/design-hs/) as
  a component library to create HTML pages.

- It also uses [`start-servant`](https://github.com/noteed/start-servant) as a
  kind of base "framework" to organize the prototype.

# Content

The application is implemented as a web server. In particular it uses the
`servant` and `stm` libraries. The `stm` library is used instead of a regular
relational database (e.g. PostgreSQL). This means the whole state of the
application is in memory instead of in the database.

In addition of the main program, some other programs are provided to better
demonstrate and explore the features of the application.

- `cty-serve` is the main program.
- `cty-repl` is similar but exposes a REPL instead of a HTTP server to run
  commands and interact with the state.
- `cty-interactive` combines both `cty-serve` and `cty-repl` so that both
  interfaces run against the same live state.
- `cty-sock` offers a text interface similar to `cty-repl` but through a
  UNIX-domain socket, and accepts multiple clients.
- `cty` is meant as a client for the `cty-sock` server. It can also be run
  against a local state file.
- `cty-parse` is a helper program to play with the command parser used in
  `cty-repl`.

**Note**: Currently, `cty-repl` and `cty-sock` use different syntax. `cty-repl`
uses a custom syntax, while `cty-sock` re-use the capabilities of the
`optparse-applicative` library. The idea is that the exact command can be
played within the interactive `cty-sock` REPL, or within Bash using the `cty`
client program.

# Running

It is possible to load the code in GHCi (but this exposes the Parse module,
instead of Main):

```
$ nix-shell
$ cabal repl prototype-hs-exe
```

Instead it's possible to use a helper script to call `:main` with GHCi.

```
$ script/run-example.sh
```

We can also use Cabal to run the example:

```
$ nix-shell
$ cabal run -- prototype-hs-exe --server-port 9000 --repl-prompt "> " --repl-history-on --repl-exit-cmd exit
```

The binary can be built and run also this way, which bypass building the tests:

```
$ cabal build prototype-hs-exe
$ ./dist-newstyle/build/x86_64-linux/ghc-8.6.5/prototype-hs-exe-0.1.0.0/x/prototype-hs-exe/build/prototype-hs-exe/prototype-hs-exe --server-port 9000 --repl-prompt "> " --repl-history-on --repl-exit-cmd exit
```

And finally, we can build a binary with Nix:

```
$ nix-build -A prototype-hs-exe
$ ./result/bin/prototype-hs-exe \
    --server-port 9000 \
    --repl-prompt "> " \
    --repl-history-on \
    --repl-exit-cmd exit
```

# Example REPL commands

The repl can be quit with `exit` (this is configurable) or `Ctrl-d`.

```
> all
> viz user UserLogin "1" "pass"
```

```
> mod user UserCreate "1" "alice" "pass"
UsersModified
    [ UserProfile
        { _userProfileId = UserId "1"
        , _userProfileName = "name"
        , _userProfilePassword = UserPassword Secret :: Text
        }
    ]
```

```
> viz user SelectUserById "1"
UsersVisualised []
```

# `cty`

`cty` is the main command-line tool to interact against a server running on the
same host (through a UNIX-domain socket), or against a state file. Note that
modifying a state file used by a running server should be avoided.

By default, `cty` interacts against a state file called `state.json`. Use the
`--state` option to override the file name. Use the `--socket` option to
instead interact against a running server.

```
$ cty init
State file 'state.json' created.

$ cty user get alice
Right (UsersVisualised [])

$ cty user create alice pass alice@example.com
Right (UsersModified [UserProfile {_userCreds = UserCreds {_userCredsId = UserId "alice", _userCredsPassword = Password Secret :: Text}, _userProfileName = "alice@example.com"}])

$ cty user get alice
Right (UsersVisualised [UserProfile {_userCreds = UserCreds {_userCredsId = UserId "alice", _userCredsPassword = Password Secret :: Text}, _userProfileName = "alice@example.com"}])

```

# Virtual machine image

A virtual machine image that can be run with QEMU can be built and run with:

```
$ nix-build -A runvm
$ result/bin/run-nixos-vm
```

Use `ctrl-a x` to quit QEMU.

An image suitable for Digital Ocean can also be built:

```
$ nix-build -A image
$ ls result/
nixos.qcow2.gz
```
