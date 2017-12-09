{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
module SchemaNew where

import Control.Applicative (Applicative, (<$>))
import Control.Lens
import Control.Monad.Haskey
import Control.Monad.Reader
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.BTree.Alloc (AllocM, AllocReaderM)
import Data.BTree.Impure (Tree, insertTree, lookupTree, toList)
import Data.BTree.Primitives (Value)
import Data.Binary (Binary)
import Data.Foldable (foldlM)
import Data.Int (Int64)
import Data.Text (Text, unpack)
import Data.Typeable (Typeable, Typeable1)
import qualified Data.BTree.Impure as Tree

import Database.Haskey.Alloc.Concurrent (Root)

import GHC.Generics (Generic, Generic1)
import Data.Functor.Classes
import UserData

--------------------------------------------------------------------------------
-- Definition of our custom schema. As well as query and modify functions.
--------------------------------------------------------------------------------

type family Tweet a
type instance Tweet CurrentSchema = TweetData
type instance Tweet OldSchema = TweetData
data TweetData = Tweet {
    _tweetUser :: !Text
  , _tweetContent :: !Text
  , _tweetReplies :: Tree Int64 (TweetData)
  } deriving (Generic, Show, Typeable)


instance Binary (TweetData)
instance Value (TweetData)


data SchemaTree a = SchemaTree {
    _schemaTweets :: Tree Int64 (Tweet a)
    , _schemaUsers :: Tree Text (User a)
    } deriving (Generic)
-- instance Show1 SchemaTree
deriving instance Show (SchemaTree CurrentSchema)
instance Binary (SchemaTree CurrentSchema)

data Schema = Schema
 { _schemaTree :: SchemaTree CurrentSchema
 }
  deriving (Generic, Typeable, Show)

instance Binary (Schema)
instance Value (Schema)
instance Root Schema

makeLenses ''SchemaTree
makeLenses ''Schema

  -- | Insert or update a tweet.
insertTweet :: AllocM n => Int64 -> Tweet CurrentSchema -> Schema -> n Schema
insertTweet k v = schemaTree . schemaTweets %%~ insertTree k v

-- | Insert a new user.
insertUser :: AllocM n => Text -> User CurrentSchema -> Schema -> n Schema
insertUser k v = schemaTree . schemaUsers %%~ insertTree k v

emptySchema :: Schema
emptySchema = Schema (SchemaTree Tree.empty Tree.empty)

code :: IO ()
code = do
    let db = "haskey"
    putStrLn $ "Using " ++ db
    main' db

main' :: FilePath -> IO ()
main' fp = do
    db <- flip runFileStoreT defFileStoreConfig $
        openConcurrentDb hnds >>= \case
            Nothing -> createConcurrentDb hnds emptySchema
            Just db -> return db

    runApp app "Hello World!" db defFileStoreConfig
  where
    hnds = concurrentHandles fp

newtype App a = AppT (ReaderT String (HaskeyT Schema IO) a)
              deriving (Functor, Applicative, Monad, MonadIO,
                        MonadHaskey Schema, MonadReader String)

runApp :: App a
       -> String
       -> ConcurrentDb Schema
       -> FileStoreConfig
       -> IO a
runApp (AppT m) r = runHaskeyT (runReaderT m r)


app :: App ()
app = insertDefaultTweets >> printTweetsWithUser

insertDefaultTweets :: App ()
insertDefaultTweets = do
    transact_ $ \schema ->
        foldlM (flip $ uncurry insertUser) schema users
        >>= commit_
    transact_ $ \schema ->
        foldlM (flip $ uncurry insertTweet) schema tweets
        >>= commit_
  where
    users = [("foo", User "Foo"),
             ("bar", User "Bar")]
    tweets = [(1, Tweet "foo" "Hey, I'm Foo!" Tree.empty),
              (2, Tweet "bar" "Hey, I'm Bar!" Tree.empty),
              (3, Tweet "foo" "I like you, Bar!" Tree.empty)]

-- | Query all tweets.
queryAllTweets :: AllocReaderM n => Schema -> n [(Int64, Tweet CurrentSchema)]
queryAllTweets root = toList (root ^. schemaTree . schemaTweets)

printTweetsWithUser :: App ()
printTweetsWithUser = do
    tweets <- map snd <$> transactReadOnly queryAllTweets
    liftIO $ print tweets
    users  <- mapM (\t -> transactReadOnly $
                     (\r -> lookupTree (_tweetUser t) (r ^. schemaTree . schemaUsers)))
                     tweets
    liftIO $ print users
  --   mapM_ print' $ zip users tweets
  -- where
  --   print' (Just user, tweet) = liftIO . putStrLn $ unpack (_userName user) ++ ": " ++ unpack (_tweetContent tweet) ++ (show $ _tweetReplies tweet)
  --   print' (Nothing  , tweet) = liftIO . putStrLn $ "?: " ++ unpack (_tweetContent tweet)
