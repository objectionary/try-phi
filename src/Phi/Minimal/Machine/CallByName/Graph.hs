{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module Phi.Minimal.Machine.CallByName.Graph where

import           Data.List                  (unfoldr)

import           Data.Graph.Inductive.Graph (DynGraph, Graph)
import qualified Data.Graph.Inductive.Graph as Graph

import           Phi.Minimal.Graph
import           Phi.Minimal.Model

type Environment = [Parent]

data Parent = Parent
  { copies   :: [(Graph.Node, Environment)]
  , original :: Graph.Node
  }

data Action
-- need to remember nodes for each action
  = DotAction (Graph.Node, Attr)
  | AppAction (Graph.Node, Environment)
  | LocAction (Graph.Node, Int)

data Configuration gr = Configuration
  { currentNode :: Maybe Graph.Node
  , actions     :: [Action]
  , environment :: Environment
  , graph       :: gr TermNode TermEdge
  }

initConfiguration :: DynGraph gr => Term -> Configuration gr
initConfiguration term = Configuration
  { actions = []
  , environment = []
  , currentNode = Just root
  , .. }
  where
   (root, graph) = toGraph term

steps :: Graph gr => Configuration gr -> [Configuration gr]
steps c = c : unfoldr (fmap dup . step) c
  where
    dup x = (x, x)

-- | Look for an attribute in parent
-- 
-- if no, go into copies

findInCurrentParent
  :: Graph gr
  => Attr -> Parent -> gr TermNode TermEdge
  -> Maybe (Either (Graph.Node, Environment) Graph.Node)
findInCurrentParent a Parent{..} graph = go copies
  where
    go ((u, env) : moreCopies) =
      case Graph.context graph u of
        (_, _, _, outEdges) ->
          case lookup (AttrEdge a) outEdges of
            Just to -> Just (Left (to, env))
            Nothing -> go moreCopies
    go [] =
      -- if not found in copies, look up in the original
      case Graph.context graph original of
        (_, _, _, outEdges) ->
          case lookup (AttrEdge a) outEdges of
            Just to -> Just (Right to)
            Nothing -> Nothing


-- IDK what's environment?
-- IDK what's parent
step :: Graph gr => Configuration gr -> Maybe (Configuration gr)
-- save pattern into conf
step conf@Configuration{..} =
  case currentNode of
    Nothing -> case actions of
      [] -> Nothing -- nothing left to do
      LocAction (_node, 0) : as -> Just conf
        { actions = as }
      LocAction (_node, n) : as
        | null environment -> Nothing -- stuck term (free locator)
        | otherwise -> Just conf
          { actions = LocAction (_node, max 0 (n - length environment)) : as -- cut locator to switch to the relevant environment
          , environment = drop n environment -- locate to the relevant environment
          }
      AppAction (_node, copy) : as ->
        case environment of
          [] -> Nothing -- should never happen?
          Parent{..} : parents -> Just conf
            { actions = as
            , environment = Parent{ copies = (_node, copy) : copies, ..} : parents
            }
      DotAction (_node, a) : as ->
        case environment of
          [] -> Nothing -- should never happen?
          parent : _parents -> -- underscore before variaable to prevent warnings and convey some info https://stackoverflow.com/a/50705888
            case findInCurrentParent a parent graph of
              Nothing -> Nothing -- runtime error?
              Just (Right to)  -> Just conf
                { currentNode = Just to
                , actions = as
                }
              Just (Left (to, newEnv)) -> Just conf -- IDK what it means
                { currentNode = Just to
                , actions = as
                , environment = newEnv
                }
    Just node -> case Graph.context graph node of
      (_, _, LocDotNode (Just n), _) -> Just conf
        { currentNode = Nothing
        , actions = LocAction (node, n) : actions
        }

      (_, _, LocDotNode Nothing, [(DotEdge a, to)]) -> Just conf
        { currentNode = Just to
        , actions = DotAction (node, a) : actions
        }
      (_, _, LocDotNode Nothing, _) -> Nothing -- should never happen!

      (_, _, ObjNode, outEdges) ->
        case lookup CopyEdge outEdges of
          Nothing -> Just conf
            { currentNode = Nothing
            , environment = Parent [] node : environment
            }
          Just to -> Just conf
            { currentNode = Just to
            , actions = AppAction (node, environment) : actions
            }

      (_, _, VoidNode, _) -> Nothing -- runtime error?
