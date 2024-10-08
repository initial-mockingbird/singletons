-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Base.CustomStar
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This file implements 'singletonStar', which generates a datatype @Rep@ and associated
-- singleton from a list of types. The promoted version of @Rep@ is kind @*@ and the
-- Haskell types themselves. This is still very experimental, so expect unusual
-- results!
--
-- See also "Data.Singletons.TH.CustomStar" from @singletons-th@, a
-- more minimal version of this module that does not re-export anything from
-- "Prelude.Singletons".
--
----------------------------------------------------------------------------

module Data.Singletons.Base.CustomStar (

  module Data.Bool.Singletons,
  module Data.Eq.Singletons,
  module Data.Singletons.Base.TH
  ) where

import Data.Bool.Singletons
import Data.Eq.Singletons
import Data.Singletons.Base.TH
