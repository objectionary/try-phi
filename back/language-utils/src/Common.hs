

module Common(ppPhiToEO, ppWHNF, ppWHNFSteps, ppNF, ppTapSteps, ppStates, ppGraphs, getTermFromPhi, getTermFromEO, ppEOSource, ppPhi, Common.ppPhiSource, ppPhiToLatex) where


import Phi.Minimal as Phi
    ( nf,
      whnf,
      Term,
      ppGraphStepsFor,
      ppStepsFor,
      ppTerm,
      ppWhnfSteps,
      ppPhiSource )

import Phi.Minimal.ConfigurationDot(renderList)
import qualified Data.Text.Lazy                       as T

import EOtoPhi(toMinimalTerm)
import EOParser as EOP(parseTermProgram)

import PhiToEO as EP (ppTermTop)

import Data.Text (pack)
import Phi.Minimal.Parser as PMP(parseTerm)
import Text.Megaparsec.Error(errorBundlePretty)
import Phi.Minimal.PPToLatex (Latex(..))

ppPhiSource :: Term -> String
ppPhiSource = show . Phi.ppPhiSource

ppEOSource :: Term -> String
ppEOSource t = (show . EP.ppTermTop) t <> "\n"

ppPhi :: Term -> String
ppPhi = show

ppPhiToEO :: Term -> String
ppPhiToEO = show . Phi.ppTerm

ppPhiToLatex :: Term -> String
ppPhiToLatex t = show (Latex t)

ppWHNF :: Term -> String
ppWHNF = show . Phi.whnf

ppNF :: Term -> String
ppNF = show . Phi.nf

ppWHNFSteps :: Term -> String
ppWHNFSteps = show . Phi.ppWhnfSteps

ppTapSteps :: Term -> String
ppTapSteps = show . Phi.ppStepsFor

-- | list of graph steps
-- FIXME send a list of steps, render current step on front
ppStates ::  Int -> Term -> [String]
ppStates lim term = show <$> Phi.ppGraphStepsFor lim term

ppGraphs :: Int -> Term -> [String]
ppGraphs lim term = T.unpack <$> renderList lim term

getTermFromEO :: String -> Either String Term
getTermFromEO s = ret
    where
        t1 = toMinimalTerm <$> parseTermProgram (pack s)
        ret =
            case t1 of
                Left l -> Left $ errorBundlePretty l
                Right r -> Right r

getTermFromPhi :: String -> Either String Term
getTermFromPhi = PMP.parseTerm

