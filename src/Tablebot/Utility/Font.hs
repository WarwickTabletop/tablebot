{-# LANGUAGE ScopedTypeVariables #-}

module Tablebot.Utility.Font (makeSansSerifEnv, FontMap, makeFontMap) where

import Control.Monad.Exception (MonadException)
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Map as M
import Graphics.Rendering.Chart.Backend.Diagrams (DEnv (..), createEnv)
import Graphics.Rendering.Chart.Backend.Types
import Graphics.SVGFonts (loadFont)
import qualified Graphics.SVGFonts.ReadFont as F
import System.Environment (lookupEnv)
import System.FilePath (replaceFileName)

-- | A type to map between some basic font characteristics and some loaded fonts.
type FontMap n = M.Map (String, FontSlant, FontWeight) (F.PreparedFont n)

makeSansSerifEnv :: forall n. (Read n, RealFloat n) => n -> n -> FontMap n -> DEnv n
makeSansSerifEnv diX diY fontMap = createEnv (AlignmentFns id id) diX diY fontSelector
  where
    alterFontFamily :: String -> F.PreparedFont n -> F.PreparedFont n
    alterFontFamily n (fd, om) = (fd {F.fontDataFamily = n}, om)
    localSansSerif :: FontMap n = M.filterWithKey (\(k, _, _) _ -> k == "sans-serif") fontMap
    localAltered :: FontMap n = M.mapWithKey (\(s, _, _) v -> alterFontFamily s v) localSansSerif
    -- we simplify the map so that other font types become sans-serif as well
    localKeySimple = M.mapKeys (\(_, fs, fw) -> (fs, fw)) localAltered
    -- we use an unsafe lookup method because what do we do if this isn't correct?
    fontSelector :: FontStyle -> F.PreparedFont n
    fontSelector FontStyle {..} = localKeySimple M.! (_font_slant, _font_weight)

makeFontMap :: (Read n, RealFloat n, MonadIO m, MonadException m) => m (FontMap n)
makeFontMap = mapM (liftIO . loadFont) localFonts

-- thanks to https://stackoverflow.com/questions/21549082/how-do-i-deploy-an-executable-using-chart-diagrams-standard-fonts-without-cabal
localFonts :: M.Map (String, FontSlant, FontWeight) FilePath
localFonts = M.fromList
        [ (("serif", FontSlantNormal, FontWeightNormal), "fonts/LinLibertine_R.svg"),
          (("serif", FontSlantNormal, FontWeightBold), "fonts/LinLibertine_RB.svg"),
          (("serif", FontSlantItalic, FontWeightNormal), "fonts/LinLibertine_RI.svg"),
          (("serif", FontSlantOblique, FontWeightNormal), "fonts/LinLibertine_RI.svg"),
          (("serif", FontSlantItalic, FontWeightBold), "fonts/LinLibertine_RBI.svg"),
          (("serif", FontSlantOblique, FontWeightBold), "fonts/LinLibertine_RBI.svg"),
          (("sans-serif", FontSlantNormal, FontWeightNormal), "fonts/SourceSansPro_R.svg"),
          (("sans-serif", FontSlantNormal, FontWeightBold), "fonts/SourceSansPro_RB.svg"),
          (("sans-serif", FontSlantItalic, FontWeightNormal), "fonts/SourceSansPro_RI.svg"),
          (("sans-serif", FontSlantOblique, FontWeightNormal), "fonts/SourceSansPro_RI.svg"),
          (("sans-serif", FontSlantItalic, FontWeightBold), "fonts/SourceSansPro_RBI.svg"),
          (("sans-serif", FontSlantOblique, FontWeightBold), "fonts/SourceSansPro_RBI.svg"),
          (("monospace", FontSlantNormal, FontWeightNormal), "fonts/SourceCodePro_R.svg"),
          (("monospace", FontSlantNormal, FontWeightBold), "fonts/SourceCodePro_RB.svg"),
          (("monospace", FontSlantItalic, FontWeightNormal), "fonts/SourceCodePro_R.svg"),
          (("monospace", FontSlantOblique, FontWeightNormal), "fonts/SourceCodePro_R.svg"),
          (("monospace", FontSlantItalic, FontWeightBold), "fonts/SourceCodePro_RB.svg"),
          (("monospace", FontSlantOblique, FontWeightBold), "fonts/SourceCodePro_RB.svg")
        ]
