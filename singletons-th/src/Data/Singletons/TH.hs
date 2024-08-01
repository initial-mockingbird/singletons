-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.TH
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module contains basic functionality for deriving your own singletons
-- via Template Haskell. Note that this module does not define any singled
-- definitions on its own. For a version of this module that comes pre-equipped
-- with several singled definitions based on the "Prelude", see
-- @Data.Singletons.Base.TH@ from the @singletons-base@ library.
--
----------------------------------------------------------------------------

module Data.Singletons.TH (
  -- * Primary Template Haskell generation functions
  singletons, singletonsOnly, genSingletons,
  promote, promoteOnly, genDefunSymbols, genPromotions,

  -- ** Functions to generate equality instances
  promoteEqInstances, promoteEqInstance,
  singEqInstances, singEqInstance,
  singDecideInstances, singDecideInstance,

  -- ** Functions to generate 'Ord' instances
  promoteOrdInstances, promoteOrdInstance,
  singOrdInstances, singOrdInstance,

  -- ** Functions to generate 'Bounded' instances
  promoteBoundedInstances, promoteBoundedInstance,
  singBoundedInstances, singBoundedInstance,

  -- ** Functions to generate 'Enum' instances
  promoteEnumInstances, promoteEnumInstance,
  singEnumInstances, singEnumInstance,

  -- ** Functions to generate 'Show' instances
  promoteShowInstances, promoteShowInstance,
  singShowInstances, singShowInstance,
  showSingInstances, showSingInstance,

  -- ** Utility functions
  singITyConInstances, singITyConInstance,

  -- * Basic singleton definitions
  module Data.Singletons,

  -- * Auxiliary definitions
  SDecide(..), (:~:)(..), Void, Refuted, Decision(..),

  SuppressUnusedWarnings(..)

 ) where


import Data.Singletons
import Data.Singletons.Decide

import Data.Singletons.TH.Promote
import Data.Singletons.TH.Single
import Data.Singletons.TH.SuppressUnusedWarnings

