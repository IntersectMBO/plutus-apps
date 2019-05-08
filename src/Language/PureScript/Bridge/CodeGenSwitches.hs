-- | General switches for the code generation, such as generating profunctor-lenses or not
module Language.PureScript.Bridge.CodeGenSwitches 
    ( Settings (..)
    , ForeignOptions(..)
    , defaultSettings
    , purs_0_11_settings
    , Switch
    , getSettings
    , defaultSwitch
    , noLenses, genLenses
    , useGen, useGenRep
    , genForeign, noForeign
    ) where


import Data.Monoid (Endo(..))

-- | General settings for code generation
data Settings = Settings
    { generateLenses :: Bool -- ^use purescript-profunctor-lens for generated PS-types?
    , genericsGenRep :: Bool -- ^generate generics using purescript-generics-rep instead of purescript-generics
    , generateForeign :: Maybe ForeignOptions -- ^generate Foreign.Generic Encode and Decode instances
    }
    deriving (Eq, Show)

data ForeignOptions = ForeignOptions
    { unwrapSingleConstructors :: Bool
    }
    deriving (Eq, Show)

-- | Settings to generate Lenses
defaultSettings :: Settings
defaultSettings = Settings True True Nothing


-- |settings for purescript 0.11.x
purs_0_11_settings :: Settings
purs_0_11_settings = Settings True False Nothing


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
noLenses = Endo $ \settings -> settings { generateLenses = False }


-- | Switch on the generatation of profunctor-lenses
genLenses :: Switch
genLenses = Endo $ \settings -> settings { generateLenses = True }


-- | Generate generics using purescript-generics-rep
useGenRep :: Switch
useGenRep = Endo $ \settings -> settings { genericsGenRep = True }


-- | Generate generics using purescript-generics
useGen :: Switch
useGen = Endo $ \settings -> settings { genericsGenRep = False }

genForeign :: ForeignOptions -> Switch
genForeign opts = Endo $ \settings -> settings { generateForeign = Just opts }

noForeign :: Switch
noForeign = Endo $ \settings -> settings { generateForeign = Nothing }