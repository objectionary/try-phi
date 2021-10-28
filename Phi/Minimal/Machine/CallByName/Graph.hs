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
  = DotAction Attr
  | AppAction (Graph.Node, Environment)
  | LocAction Int

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
steps c = c : unfoldr (\conf -> dup <$> step conf) c
  where
    dup x = (x, x)

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
      case Graph.context graph original of
        (_, _, _, outEdges) ->
          case lookup (AttrEdge a) outEdges of
            Just to -> Just (Right to)
            Nothing -> Nothing

step :: Graph gr => Configuration gr -> Maybe (Configuration gr)
step conf@Configuration{..} =
  case currentNode of
    Nothing -> case actions of
      [] -> Nothing -- nothing left to do
      LocAction 0 : as -> Just conf
        { actions = as }
      LocAction n : as
        | null environment -> Nothing -- stuck term (free locator)
        | otherwise -> Just conf
          { actions = LocAction (max 0 (n - length environment)) : as
          , environment = drop n environment
          }
      AppAction copy : as ->
        case environment of
          [] -> Nothing -- should never happen?
          Parent{..} : parents -> Just conf
            { actions = as
            , environment = Parent{ copies = copy : copies, ..} : parents
            }
      DotAction a : as ->
        case environment of
          [] -> Nothing -- should never happen?
          parent : _parents ->
            case findInCurrentParent a parent graph of
              Nothing -> Nothing -- runtime error?
              Just (Right to)  -> Just conf
                { currentNode = Just to
                , actions = as
                }
              Just (Left (to, newEnv)) -> Just conf
                { currentNode = Just to
                , actions = as
                , environment = newEnv
                }
    Just node -> case Graph.context graph node of
      (_, _, LocDotNode (Just n), _) -> Just conf
        { currentNode = Nothing
        , actions = LocAction n : actions
        }

      (_, _, LocDotNode Nothing, [(DotEdge a, to)]) -> Just conf
        { currentNode = Just to
        , actions = DotAction a : actions
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
