{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
module Common(ppPhiToEO, ppWHNF, ppWHNFSteps, ppNF, ppTapSteps, ppStates, ppGraphs, getTermFromPhi, getTermFromEO, ppEO, ppPhi, Common.ppPhiSource) where


import Phi.Minimal as Phi

import Phi.Minimal.ConfigurationDot(renderAsDot)
import qualified Data.Text.Lazy                       as T
import           Data.Graph.Inductive.PatriciaTree    (Gr)
import qualified Phi.Minimal.Machine.CallByName.Graph as CGraph
import Phi.Minimal.Model as PM

import EO.EOtoPhi(toMinimalTerm)
import EOParser as EOP(parseTermProgram)

import Phi.Minimal.EO.Pretty as EP (ppTerm)

import Data.Text (pack)
import Phi.Minimal.Parser as PMP(parseTerm)
import Text.Megaparsec.Error(errorBundlePretty)
-- import Phi.Minimal.Print(ppPhiSource)

-- TODO send html, not text
-- import Lucid (Html, ToHtml (toHtml), br_, pre_)

-- import Prettyprinter.Render.Util.SimpleDocTree (SimpleDocTree (..))

-- renderHtml :: SimpleDocTree (Html () -> Html ()) -> Html ()
-- renderHtml =
--     let go = \case
--             STEmpty -> pure ()
--             STChar c -> toHtml $ T.singleton c
--             STText _ t -> toHtml t
--             STLine i -> br_ [] >> toHtml (T.replicate i $ T.singleton ' ')
--             STAnn ann content -> ann $ go content
--             STConcat contents -> foldMap go contents
--      in pre_ . go

ppPhiSource :: Term -> String
ppPhiSource = show . Phi.ppPhiSource

ppEO :: Term -> String
ppEO t = (show . EP.ppTerm) t <> "\n"

ppPhi :: Term -> String
ppPhi = show

ppPhiToEO :: Term -> String
ppPhiToEO = show . Phi.ppTerm

ppWHNF :: Term -> String
ppWHNF = show . Phi.whnf

ppNF :: Term -> String
ppNF = show . Phi.nf

ppWHNFSteps :: Term -> String
ppWHNFSteps = show . Phi.ppWhnfSteps

ppTapSteps :: Term -> String
ppTapSteps = show . Phi.ppStepsFor

-- | list of graph steps
ppStates :: Term -> Int -> [String]
ppStates
    term
    lim
    = show . Phi.ppGraphStepsFor term <$> [0 .. lim]

ppGraphs :: PM.Term -> Int -> [String]
ppGraphs term lim = T.unpack <$> map (renderAsDot @Gr) (take lim $ CGraph.steps $ CGraph.initConfiguration term)

-- FIXME use Either
-- getTermFromEO :: String -> Either  PM.Term
-- getTermFromEO :: String -> Either (ParseErrorBundle Text Void) Term
getTermFromEO :: String -> Either String Term
getTermFromEO s = ret
    where
        t = toMinimalTerm <$> parseTermProgram (pack s)
        ret =
            case t of
                Left l -> Left $ errorBundlePretty l
                Right r -> Right r

getTermFromPhi :: String -> Either String Term
getTermFromPhi = PMP.parseTerm

