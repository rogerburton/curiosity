#! /usr/bin/env nix-shell
#! nix-shell -i bash ../shell.nix

# cabal repl doesn't do what I want. In particular it loads and only exposes
# the Parse module.
#
# The flags are copied from what `cabal repl -v` does.  I'm not sure if
# everything is necessary. I think we need at least the explicit language
# extensions and the no-prelude bits. This in turn requires us to add packages
# explicitely ?

ghc --interactive \
  -iexample/executable/ \
  -hide-all-packages \
  -Wmissing-home-modules \
  -no-user-package-db \
  -package-db /home/thu/.cabal/store/ghc-8.6.5/package.db \
  -package-db /home/thu/work/smart/prototype-hs/dist-newstyle/packagedb/ghc-8.6.5 \
  -package-db /home/thu/work/smart/prototype-hs/dist-newstyle/build/x86_64-linux/ghc-8.6.5/prototype-hs-example-0.1.0.0/x/prototype-hs-example-exe/package.conf.inplace \
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
  -package prototype-hs-example \
  -package prototype-hs-lib \
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
