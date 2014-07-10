{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
module DataSourceExample
       (PersonId(..)
       ,Person(..)
       ,getPerson
       ,initHaxlState) where

import           Control.Applicative                  ((<$>), (<*>))
import           Control.Concurrent.Async
import           Control.Exception
import           Data.Hashable
import           Data.Maybe                           (listToMaybe)
import           Data.Text                            (Text)
import           Data.Typeable
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.FromRow
import           Haxl.Core

newtype PersonId = PersonId Int deriving (Show, Eq, FromField)

instance FromRow Person where
  fromRow = Person <$> field
                   <*> field
                   <*> field
                   <*> field

data Person = Person { _id        :: PersonId
                     , first_name :: Text
                     , last_name  :: Text
                     , age        :: Int } deriving (Show, Typeable)

-- | PGReq GADT
-- GADTs, DeriveDataTypeable
data PGReq a where
  GetPerson :: PersonId -> PGReq (Maybe Person)
  deriving Typeable

-- | GADT Instances
-- requires StandaloneDeriving
deriving instance Eq (PGReq a)
deriving instance Show (PGReq a)

instance Show1 PGReq where show1 = show

instance Hashable (PGReq a) where
  hashWithSalt s (GetPerson (PersonId pid)) = hashWithSalt s (0::Int, pid)

-- | Data Source State
-- needs TypeFamilies
instance StateKey PGReq where
  data State PGReq =
    PGState
      { connInfo :: ConnectInfo }

initHaxlState
  :: ConnectInfo
  -> IO (State PGReq)
initHaxlState cInfo = do
  return PGState
    { connInfo = cInfo }

-- | DataSource Instances
instance DataSourceName PGReq where
  dataSourceName _ = "Postgres"

instance DataSource u PGReq where
  fetch = pgFetch

-- | Fetch
-- require REcordWildCards
pgFetch
 :: State PGReq
 -> Flags
 -> u
 -> [BlockedFetch PGReq]
 -> PerformFetch
pgFetch PGState {..} _flags _user bfs =
  AsyncFetch $ \inner -> do
    asyncs <- mapM (fetchAsync connInfo) bfs
    inner
    mapM_ wait asyncs

fetchAsync
  :: ConnectInfo
  -> BlockedFetch PGReq
  -> IO (Async ())
fetchAsync creds (BlockedFetch req rvar) =
  async $ do
    bracket (connect creds) (close) $ \conn -> do
      e <- Control.Exception.try $ fetchReq conn req
      case e of
        Left ex -> putFailure rvar (ex :: SomeException)
        Right val -> putSuccess rvar val

fetchReq
  :: Connection
  -> PGReq a
  -> IO a
fetchReq conn (GetPerson (PersonId pid)) = do
  people <- query conn "select * from people where _id = ?" (Only pid) :: IO [Person]
  return $ listToMaybe people

-- | User funcs
getPerson :: PersonId -> GenHaxl u (Maybe Person)
getPerson pid = dataFetch (GetPerson pid)
