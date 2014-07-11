{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Lens
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.State
import           Data.ByteString            (ByteString)
import           Data.Lens.Common           (getL)
import           Data.Maybe                 (fromMaybe)
import qualified Data.Text                  as T
import           Database.PostgreSQL.Simple
import           DataSourceExample
import           Haxl.Core
import           Heist
import qualified Heist.Interpreted          as I
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Haxl
import           Snap.Snaplet.Heist
import           Snap.Util.FileServe
------------------------------------------------------------------------------
import           Application
------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [("",          serveDirectory "static")
         ,("testhaxl",  with haxl $ haxlHandler)]

haxlHandler :: Handler b (HaxlEnv ()) ()
haxlHandler = do
  person <-  withHaxl $ getPerson (PersonId 1)
  writeText $ maybe "" (first_name) person

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    addRoutes routes
    let cinfo = defaultConnectInfo {connectUser = "pgsuper", connectPassword = "password", connectDatabase = "peopledb"}
    pgstate <- liftIO $ initHaxlState cinfo
    henv <- liftIO $ initEnv (stateSet pgstate stateEmpty) ()
    hax <- nestSnaplet "" haxl $ haxlInit henv
    return $ App h hax
