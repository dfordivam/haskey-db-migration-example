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
module UserData where

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

data CurrentSchema = CurrentSchema
  deriving (Show)
data OldSchema = OldSchema
  deriving (Show)


type family User a
type instance User CurrentSchema = UserData
type instance User OldSchema = UserData
data UserData = User {
    _userName :: !Text
  } deriving (Generic, Show, Typeable)


instance Binary (UserData)
instance Value (UserData)
