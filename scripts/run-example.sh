#! /usr/bin/env nix-shell
#! nix-shell -i bash ../shell.nix

# See ghci.sh for comments.

ghc --interactive \
  -i../smart-design-hs/lib/src/ \
  -iexe/executable/ \
  -iexe/src/ \
  -ilib/src/ \
  -hide-package base \
  -XNoImplicitPrelude \
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
  -ghci-script scripts/ghci-run.conf
