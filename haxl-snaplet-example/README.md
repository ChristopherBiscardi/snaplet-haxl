# Snaplet-Haxl example application

Relevant tables and such are located [in this blog post](http://www.christopherbiscardi.com/2014/07/04/a-foray-into-haxl-postgresql-simple/)

Probably most interesting is the initialization code:

```haskell
let cinfo = defaultConnectInfo {connectUser = "pgsuper", connectPassword = "password", connectDatabase = "peopledb"}
pgstate <- liftIO $ initHaxlState cinfo
henv <- liftIO $ initEnv (stateSet pgstate stateEmpty) ()
hax <- nestSnaplet "" haxl $ haxlInit henv
```

# Does it work?

Yes! Give the example a try!

```
cd haxl-snaplet-example
cabal sandbox init
cabal install --dependencies-only
cabal build
./dist/build/haxl-snaplet-example/haxl-snaplet-example
```
