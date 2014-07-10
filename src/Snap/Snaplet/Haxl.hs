{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Snap.Snaplet.Haxl
       (haxlInit
       ,withHaxl
       ,HaxlEnv(..)
       ,HasHaxl(..)) where

import           Control.Monad.CatchIO      (MonadCatchIO)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.State        (get)
import           Control.Monad.Trans.Reader (ReaderT, ask)
import           Data.Typeable
import           Database.PostgreSQL.Simple
import           Haxl.Core
import qualified Haxl.Core.Env              as E
import           Snap

-- data HaxlSnap a where
--   InitEnv :: (u -> IO (E.Env u)) -> HaxlSnap E.Env

-- newtype Henv u = Env u

data HaxlEnv u = HaxlEnv {en :: E.Env u}

class (MonadCatchIO m) => HasHaxl m where
  getHaxlState :: m (HaxlEnv u)

-- instance HasHaxl (Handler b (HaxlEnv u)) where
--         getHaxlState = get

-- instance (MonadCatchIO m) => HasHaxl (ReaderT (HaxlEnv u) m) where
--         getHaxlState = ask


haxlInit :: E.Env e ->  SnapletInit b (HaxlEnv e)
haxlInit e = makeSnaplet "haxl" "Simple Haxl Snaplet" Nothing $ do
  return $ HaxlEnv e

withHaxl :: (HasHaxl m) => GenHaxl u b -> m (b)
withHaxl f = do
  envir <- getHaxlState
  liftIO $ runHaxl (en envir) $ do f

-- withH :: SnapletLens b (HaxlEnv u) -> Handler b v a -> Handler b v a
-- withH l h = do
--   a <- h
--   withTop l $ do
--     envir <- get
--     liftIO $ runHaxl (en envir) a

-- haxlInit' :: SnapletLens b v -> Haxl.Core.State a -> SnapletInit b (HaxlEnv b)
-- haxlInit' l state = do
--   makeSnaplet "haxl" "Simple Haxl Snaplet" Nothing $ do
--     e <- liftIO $ (initEnv (stateSet state stateEmpty) l)
--     return $ HaxlEnv e
