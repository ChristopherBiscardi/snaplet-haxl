{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.Haxl
       (haxlInit
       ,withHaxl
       ,HaxlEnv(..)
       ,HasHaxl(..)) where

import           Control.Monad.CatchIO      (MonadCatchIO)
import           Control.Monad.Trans.Reader (ReaderT, ask)
import           Haxl.Core                  (GenHaxl, runHaxl)
import qualified Haxl.Core.Env              as E (Env)
import           Snap                       (Handler, SnapletInit, get, liftIO, makeSnaplet)

data HaxlEnv u = HaxlEnv {en :: E.Env u}

class (MonadCatchIO m) => HasHaxl m where
  getHaxlState :: m (HaxlEnv ())

instance HasHaxl (Handler b (HaxlEnv ())) where
        getHaxlState = get

instance (MonadCatchIO m) => HasHaxl (ReaderT (HaxlEnv ()) m) where
        getHaxlState = ask


haxlInit :: E.Env e ->  SnapletInit b (HaxlEnv e)
haxlInit e = makeSnaplet "haxl" "Simple Haxl Snaplet" Nothing $ do
  return $ HaxlEnv e

withHaxl :: (HasHaxl m) => GenHaxl () b -> m (b)
withHaxl f = do
  envir <- getHaxlState
  liftIO $ runHaxl (en envir) $ do f
