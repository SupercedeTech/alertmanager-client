{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Network.Alertmanager.Client.Types.Internal
  ( makeFieldLenses
  ) where

import Control.Lens.Operators
import Control.Lens.TH

unexportedFieldLenses :: LensRules
unexportedFieldLenses = lensRules
  & lensField .~ mappingNamer pure

-- Deliberately omitting the type signature because we don't want to have to
-- import TH modules and introduce an extra dependency.

-- | Generate lenses for a type, using the same name for the lenses as are
-- given to the fields themselves.
--
-- This will cause conflicting definition errors if you use it wrong - this
-- is intentional! This is meant for record types which hog good lens names
-- with their field names - and should only be used when those types are
-- imported in such a way that the field names are not in scope. For example,
-- it can be used to re-export a record, hiding the constructor and fields,
-- but exposing lenses instead.
makeFieldLenses = makeLensesWith unexportedFieldLenses
