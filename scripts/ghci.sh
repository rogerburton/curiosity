#! /usr/bin/env bash

# cabal repl doesn't do what I want. In particular it loads and only exposes
# the Parse module.
#
# The flags are copied from what `cabal repl -v` does.  I'm not sure if
# everything is necessary. I think we need at least the explicit language
# extensions and the no-prelude bits. This in turn requires us to add packages
# explicitely ?

ghc --interactive \
  -iexample/executable/ \
  -hide-all-packages -Wmissing-home-modules -no-user-package-db -package-db /home/thu/.cabal/store/ghc-8.6.5/package.db -package-db /home/thu/work/smart/prototype-hs/dist-newstyle/packagedb/ghc-8.6.5 -package-db /home/thu/work/smart/prototype-hs/dist-newstyle/build/x86_64-linux/ghc-8.6.5/prototype-hs-example-0.1.0.0/x/prototype-hs-example-exe/package.conf.inplace -package-id 'start-servant-0.1.0.0-2UOR3nYE030KpKThol2Hib (Prototype.ACL, Prototype.ACL.Types, Prototype.Data.Examples, Prototype.Grantee.Group, Prototype.Html, Prototype.Module, Prototype.Pages.Home, Prototype.Runtime, Prototype.Runtime.Errors, Prototype.Runtime.StmDatabase, Prototype.Runtime.Storage, Prototype.Server.Legacy, Prototype.Server.Legacy.Auth, Prototype.Server.New, Prototype.Server.New.Auth, Prototype.Server.New.Page, Prototype.Server.New.Page.Shared, Prototype.Server.New.Page.Shared.ViewMode, Prototype.Server.New.Page.UserPages, Prototype.Server.New.Page.UserPages.Todos, Prototype.Server.New.Page.UserPages.Todos.Item, Prototype.Server.New.StartPage, Prototype.Server.New.Todos, Prototype.Types, Prototype.Types.NonEmptyText, Prototype.Types.Secret)' -package-id async-2.2.2-6rOJAIn6doQ8bltUxcy0dW -package-id base-noprelude-4.12.0.0-LO8XhqS01WD4E3cSCpqQ2C -package-id containers-0.6.0.1 -package-id data-default-class-0.1.2.0-FeIQ5tLoVZBHMSgrT9zptQ -package-id http-types-0.12.3-F2xMhplkW5dAYXtwWx85Jd -package-id lens-4.17.1-OI9xiiJY6KIEwKnz7hWvu -package-id megaparsec-7.0.5-FFzGz4dCh1H9yigxOvQWbt -package-id optparse-applicative-0.14.3.0-KgUFYVq9ZZSIX6QVzTkb7A -package-id pretty-simple-2.2.0.1-3sR90XMyE51FkTGrLwHR9i -package-id protolude-0.2.3-7xU7UxDZe4gBg92V9qpGfW -package-id prototype-hs-example-0.1.0.0-inplace -package-id prototype-hs-lib-0.1.0.0-inplace -package-id stm-2.5.0.0 -package-id text-1.2.3.1 \
  -XHaskell2010 -XStrictData -XMultiParamTypeClasses -XDerivingStrategies -XDerivingVia -XDeriveGeneric -XRecordWildCards -XTypeSynonymInstances -XFlexibleInstances -XFlexibleContexts -XUndecidableInstances -XLambdaCase -XTypeApplications -XScopedTypeVariables -XGADTs -XOverloadedStrings -XPackageImports example/executable/ExampleMain.hs
