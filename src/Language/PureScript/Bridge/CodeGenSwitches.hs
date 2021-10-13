-- | General switches for the code generation, such as generating profunctor-lenses or not
module Language.PureScript.Bridge.CodeGenSwitches
  ( Settings(..)
  , defaultSettings
  , Switch
  , getSettings
  , defaultSwitch
  , noLenses
  , genLenses
  ) where

import Data.Monoid (Endo(..))

-- | General settings for code generation
newtype Settings =
  Settings
    { generateLenses :: Bool -- ^use purescript-profunctor-lens for generated PS-types?
    }
  deriving (Eq, Show)

-- | Settings to generate Lenses
defaultSettings :: Settings
defaultSettings = Settings True

-- | you can `mappend` switches to control the code generation
type Switch = Endo Settings

-- | Translate switches into settings
getSettings :: Switch -> Settings
getSettings switch = appEndo switch defaultSettings

-- | Default switches include code generation for lenses
defaultSwitch :: Switch
defaultSwitch = mempty

-- | Switch off the generatation of profunctor-lenses
noLenses :: Switch
noLenses = Endo $ \settings -> settings {generateLenses = False}

-- | Switch on the generatation of profunctor-lenses
genLenses :: Switch
genLenses = Endo $ \settings -> settings {generateLenses = True}
