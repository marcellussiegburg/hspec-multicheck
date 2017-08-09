{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Test.Hspec.Multicheck (
  module Test.Hspec,
  (==>), it, property, specify
  ) where

import qualified Test.QuickCheck       as QC
import qualified Test.SmallCheck       as SC
import qualified Test.Hspec.SmallCheck as SC
import           Test.Hspec            hiding (it, specify)
import qualified Test.Hspec            as Hspec

import Prelude hiding (length)

it :: (QC.Testable prop, SC.Testable IO prop) => String -> prop -> Spec
it descr p = do
  Hspec.it (descr ++ " (SC)") $ SC.property p
  Hspec.it (descr ++ " (QC)") $ QC.property p

specify :: (QC.Testable prop, SC.Testable IO prop) => String -> prop -> Spec
specify = it

infixr 0 ==>
(==>) :: (SC.Testable m prop, QC.Testable prop)
  => Bool -> prop -> (SC.Property m, QC.Property)
cond ==> p = (cond SC.==> p, cond QC.==> p)

property :: (SC.Testable IO prop, QC.Testable prop)
  => prop -> (SC.Property IO, QC.Property)
property p = (SC.property p, QC.property p)

instance QC.Testable (SC.Property m, QC.Property) where
  property (_, p) = QC.property p

instance (Monad m, m ~ n) => SC.Testable n (SC.Property m, QC.Property) where
  test (p, _) = SC.test p
