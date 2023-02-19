module Tablebot.Utility.Font (makeSansSerifvEnv) where

import qualified Data.Map as M
import Graphics.Rendering.Chart.Backend.Diagrams (DEnv (..), createEnv)
import Graphics.Rendering.Chart.Backend.Types
import Graphics.SVGFonts (loadFont)
import qualified Graphics.SVGFonts.ReadFont as F
import System.Environment (lookupEnv)
import System.FilePath (replaceFileName)
import Tablebot.Plugins.Roll.Dice.DiceEval (evaluationException)

makeSansSerifvEnv :: (Read n, RealFloat n) => n -> n -> IO (DEnv n)
makeSansSerifvEnv diX diY = do
  exec <- lookupEnv "FONT_PATH"
  case exec of
    Nothing -> evaluationException "cannot find FONT_PATH env variable" []
    Just exec' -> do
      -- we only care about sans-serif, since that's the default.
      -- this is done here to prevent having to load all the fonts
      let local = M.filterWithKey (\(nm, _, _) _ -> nm == "sans-serif") $ localFonts exec'
      localLookedUp <- mapM loadFont local
      let localAltered = M.mapWithKey (\(s, _, _) v -> alterFontFamily s v) localLookedUp
          -- we simplify the map so that other font types become sans-serif as well
          localKeySimple = M.mapKeys (\(_, fs, fw) -> (fs, fw)) localAltered
          -- we use an unsafe lookup method because what do we do if this isn't correct?
          fontSelector FontStyle {..} = localKeySimple M.! (_font_slant, _font_weight)
      pure $ createEnv (AlignmentFns id id) diX diY fontSelector
  where
    alterFontFamily :: String -> F.PreparedFont n -> F.PreparedFont n
    alterFontFamily n (fd, om) = (fd {F.fontDataFamily = n}, om)

-- thanks to https://stackoverflow.com/questions/21549082/how-do-i-deploy-an-executable-using-chart-diagrams-standard-fonts-without-cabal
localFonts :: FilePath -> M.Map (String, FontSlant, FontWeight) FilePath
localFonts exec =
  let serifR = replaceFileName exec "fonts/LinLibertine_R.svg"
      serifRB = replaceFileName exec "fonts/LinLibertine_RB.svg"
      serifRBI = replaceFileName exec "fonts/LinLibertine_RBI.svg"
      serifRI = replaceFileName exec "fonts/LinLibertine_RI.svg"
      sansR = replaceFileName exec "fonts/SourceSansPro_R.svg"
      sansRB = replaceFileName exec "fonts/SourceSansPro_RB.svg"
      sansRBI = replaceFileName exec "fonts/SourceSansPro_RBI.svg"
      sansRI = replaceFileName exec "fonts/SourceSansPro_RI.svg"
      monoR = replaceFileName exec "fonts/SourceCodePro_R.svg"
      monoRB = replaceFileName exec "fonts/SourceCodePro_RB.svg"
   in M.fromList
        [ (("serif", FontSlantNormal, FontWeightNormal), serifR),
          (("serif", FontSlantNormal, FontWeightBold), serifRB),
          (("serif", FontSlantItalic, FontWeightNormal), serifRI),
          (("serif", FontSlantOblique, FontWeightNormal), serifRI),
          (("serif", FontSlantItalic, FontWeightBold), serifRBI),
          (("serif", FontSlantOblique, FontWeightBold), serifRBI),
          (("sans-serif", FontSlantNormal, FontWeightNormal), sansR),
          (("sans-serif", FontSlantNormal, FontWeightBold), sansRB),
          (("sans-serif", FontSlantItalic, FontWeightNormal), sansRI),
          (("sans-serif", FontSlantOblique, FontWeightNormal), sansRI),
          (("sans-serif", FontSlantItalic, FontWeightBold), sansRBI),
          (("sans-serif", FontSlantOblique, FontWeightBold), sansRBI),
          (("monospace", FontSlantNormal, FontWeightNormal), monoR),
          (("monospace", FontSlantNormal, FontWeightBold), monoRB),
          (("monospace", FontSlantItalic, FontWeightNormal), monoR),
          (("monospace", FontSlantOblique, FontWeightNormal), monoR),
          (("monospace", FontSlantItalic, FontWeightBold), monoRB),
          (("monospace", FontSlantOblique, FontWeightBold), monoRB)
        ]
