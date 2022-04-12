#! /usr/bin/env nix-shell
#! nix-shell -i bash ../shell.nix

# See ghci.sh for comments.

ghc --interactive \
  -iexample/executable/ \
  -iexample/src/ \
  -ilib/src/ \
  -hide-all-packages \
  -Wmissing-home-modules \
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
  example/executable/ExampleMain.hs \
  -e ":main --server-port 9000 --repl-prompt \"> \" --repl-history-on --repl-exit-cmd exit"
