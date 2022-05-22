#! /usr/bin/env nix-shell
#! nix-shell -i bash ../shell.nix

# This is a bit convoluted because the Prelude is from design-hs:
#   - -XNoImplicitPrelude is set here on the command-line
#   - The ghci.conf file will load design-hs's Prelude
#   - Then set again -XImplicitPrelude
#   - Then load what we want: cty.hs

ghc --interactive \
  -i../design-hs/lib/src/ \
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
  -ghci-script scripts/ghci-cty.conf
