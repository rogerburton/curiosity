* Prototyping Smart

This repository contains a prototype for the
[[https://github.com/smartcoop/][Smart]] cooperative. What we hear about
"prototype" is written in =start-servant='s README.

** Goals

- Use and exemplify the use of
  [[https://github.com/smartcoop/design-hs/][design-hs]] as a design
  component library.
- Use and exemplify the use of
  [[https://github.com/noteed/start-servant][start-servant]]
- Ensure prototyping any real-life aspect of the Smart system is easy
  and quick.

** Running

It is possible to load the code in GHCi (but this exposes the Parse module,
instead of Main):

```
$ nix-shell
$ cabal repl prototype-hs-example-exe
```

Instead it's possible to use a helper script to call `:main` with GHCi.

```
$ script/run-example.sh
```

We can also use Cabal to run the example:

```
$ nix-shell
$ cabal run -- prototype-hs-example-exe --server-port 9000 --repl-prompt "> " --repl-history-on --repl-exit-cmd exit
```

The binary can be built and run also this way, which bypass building the tests:

```
$ cabal build prototype-hs-example-exe
$ ./dist-newstyle/build/x86_64-linux/ghc-8.6.5/prototype-hs-example-0.1.0.0/x/prototype-hs-example-exe/build/prototype-hs-example-exe/prototype-hs-example-exe --server-port 9000 --repl-prompt "> " --repl-history-on --repl-exit-cmd exit
```

And finally, we can build a binary with Nix:

```
$ nix-build -A prototype-hs-example
$ ./result/bin/prototype-hs-example-exe \
    --server-port 9000 \
    --repl-prompt "> " \
    --repl-history-on \
    --repl-exit-cmd exit
```

** Example REPL commands

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

** Virtual machine image

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
