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
module Schema where

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

--------------------------------------------------------------------------------
-- Definition of our custom schema. As well as query and modify functions.
--------------------------------------------------------------------------------

data family Tweet a
data instance Tweet CurrentSchema = Tweet {
    _tweetUser :: !Text
  , _tweetContent :: !Text
  , _tweetReplies :: Tree Int64 (Tweet CurrentSchema)
  } deriving (Generic, Show)

deriving instance Typeable1 Tweet
instance Binary (Tweet a)
instance Value (Tweet a)

data family User a
data instance User CurrentSchema = User {
    _userName :: !Text
  , _userEmail :: !Text
  } deriving (Generic, Typeable)

data instance User OldSchema = UserOld {
    _userNameOld :: !Text
  , _userEmailOld :: Maybe Text
  } deriving (Generic, Typeable)

instance Binary (User a)
instance Value (User a)
instance Show (User a)

data SchemaTree a = Schema {
  --   _schemaTweets :: Tree Int64 (Tweet a)
  -- ,
    _schemaUsers :: Tree Text (User a)
  } deriving (Generic1, Show, Functor)

deriving instance Typeable1 SchemaTree
type Schema = SchemaTree CurrentSchema
type SchemaOld = SchemaTree OldSchema

data CurrentSchema = CurrentSchema
data OldSchema = OldSchema

instance Binary Schema
instance Value Schema
instance Root Schema

instance Binary SchemaOld
instance Value SchemaOld
instance Root SchemaOld

-- emptySchema :: Schema
-- emptySchema = Schema Tree.empty Tree.empty

makeLenses ''SchemaTree
-- makeLenses ''(User CurrentSchema)
-- makeLenses ''(User OldSchema)
-- makeLenses ''(Tweet a)
