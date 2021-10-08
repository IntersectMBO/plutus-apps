-- | General switches for the code generation, such as generating profunctor-lenses or not
module Language.PureScript.Bridge.CodeGenSwitches
  ( Settings(..)
  , ArgonautOptions(..)
  , defaultSettings
  , Switch
  , getSettings
  , defaultSwitch
  , noLenses
  , genLenses
  , genArgonaut
  , noArgonaut
  ) where

import Data.Monoid (Endo(..))

-- | General settings for code generation
data Settings =
  Settings
    { generateLenses :: Bool -- ^use purescript-profunctor-lens for generated PS-types?
    , generateArgonaut :: Maybe ArgonautOptions -- ^generate Argonaut EncodeJson and DecodeJson instances
    }
  deriving (Eq, Show)

data ArgonautOptions =
  ArgonautOptions
    -- { unwrapSingleConstructors :: Bool
    -- }
  deriving (Eq, Show)

-- | Settings to generate Lenses
defaultSettings :: Settings
defaultSettings = Settings True Nothing

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

-- | Switch on the generatation of argonaut decode and encode instances
genArgonaut :: ArgonautOptions -> Switch
genArgonaut opts = Endo $ \settings -> settings {generateArgonaut = Just opts}

-- | Switch off the generatation of argonaut decode and encode instances
noArgonaut :: Switch
noArgonaut = Endo $ \settings -> settings {generateArgonaut = Nothing}
