{-# LANGUAGE CPP, StandaloneDeriving, DeriveDataTypeable #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.STM.SSemInternals
-- Copyright   :  (c) Chris Kuklewicz, 2012
-- License     :  BSD-style
-- 
-- Maintainer  :  haskell@list.mightyreason.com
-- Stability   :  experimental
-- Portability :  non-portable (concurrency)
--
-- Very simple quantity semaphore. Declared here so that private constructor
-- can be shared in both STM and IO APIs but hidden from user.
--
-----------------------------------------------------------------------------

module Control.Concurrent.STM.SSemInternals(SSem(SSem)) where

import Control.Concurrent.STM.TVar(TVar)
import Data.Typeable(Typeable) -- Typeable(typeOf),TyCon,mkTyCon,mkTyConApp)

#include "Typeable.h"

newtype SSem = SSem (TVar Int)  deriving (Eq)

INSTANCE_TYPEABLE0(SSem,semTc,"SSem")
