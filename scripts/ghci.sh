#! /usr/bin/env nix-shell
#! nix-shell -i bash ../shell.nix

# cabal repl doesn't do what I want. In particular it loads and only exposes
# the Parse module.
#
# The flags are copied from what `cabal repl -v` does.  I'm not sure if
# everything is necessary. I think we need at least the explicit language
# extensions and the no-prelude bits. This in turn requires us to add packages
# explicitely ?
#
#   $ rm -r dist-newstyle/
#   $ nix-shell --run 'cabal build --only-dependencies prototype-hs-example'
#   $ scripts/ghci.sh

ghc --interactive \
  -iexample/executable/ \
  -iexample/src/ \
  -ilib/src/ \
  -hide-all-packages \
  -Wmissing-home-modules \
  -no-user-package-db \
  -package-db ./dist-newstyle/packagedb/ghc-8.6.5 \
  -package start-servant \
  -package async \
  -package base-noprelude \
  -package containers \
  -package data-default-class \
  -package http-types \
  -package lens \
  -package megaparsec \
  -package optparse-applicative \
  -package pretty-simple \
  -package protolude \
  -package readline \
  -package stm \
  -package text \
  -XHaskell2010 \
  -XStrictData \
  -XMultiParamTypeClasses \
  -XDerivingStrategies \
  -XDerivingVia \
  -XDeriveGeneric \
  -XRecordWildCards \
  -XTypeSynonymInstances \
  -XFlexibleInstances \
  -XFlexibleContexts \
  -XUndecidableInstances \
  -XLambdaCase \
  -XTypeApplications \
  -XScopedTypeVariables \
  -XGADTs \
  -XOverloadedStrings \
  -XPackageImports \
  example/executable/ExampleMain.hs
