{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
module Common(ppPhiToEO, ppWHNF, ppWHNFSteps, ppNF, ppTapSteps, ppStates, ppGraphs, getTermFromPhi, getTermFromEO) where


import Phi.Minimal as Phi

import Phi.Minimal.ConfigurationDot(renderAsDot)
import qualified Data.Text.Lazy                       as T
import           Data.Graph.Inductive.PatriciaTree    (Gr)
import qualified Phi.Minimal.Machine.CallByName.Graph as CGraph
import Phi.Minimal.Model as PM

import EO.EOtoPhi(toMinimalTerm)
import EOParser(parseTermProgram)

import Data.Text (Text, pack)
import Phi.Minimal.Parser as PMP(parseTerm)

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

tShow :: (Show a) => a -> Text
tShow = pack . show

ppPhiToEO :: Term -> Text
ppPhiToEO = tShow . Phi.ppTerm

ppWHNF :: Term -> Text
ppWHNF = tShow . Phi.whnf

ppNF :: Term -> Text
ppNF = tShow . Phi.nf

ppWHNFSteps :: Term -> Text
ppWHNFSteps = tShow . Phi.ppWhnfSteps

ppTapSteps :: Term -> Text
ppTapSteps = tShow . Phi.ppStepsFor

-- | list of graph steps
ppStates :: Term -> Int -> [Text]
ppStates
    term 
    lim 
    = tShow . Phi.ppGraphStepsFor term <$> [0 .. lim]

ppGraphs :: PM.Term -> Int -> [Text]
ppGraphs term lim = T.toStrict <$> map (renderAsDot @Gr) (take lim $ CGraph.steps $ CGraph.initConfiguration term)

-- FIXME use Either
getTermFromEO :: String -> Maybe PM.Term
getTermFromEO s = toMinimalTerm <$> parseTermProgram (pack s)

getTermFromPhi :: String -> Either String Term
getTermFromPhi = PMP.parseTerm