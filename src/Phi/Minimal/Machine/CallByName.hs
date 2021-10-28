{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module Phi.Minimal.Machine.CallByName where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.List           (unfoldr)

import           Phi.Minimal.Model

type Environment = [Parent]

data Parent = Parent
  { original     :: Object Term
  , applications :: HashMap Attr (Term, Environment)
  }

data Action
  = DotAction Attr
  | AppAction (Attr, (Term, Environment))

data Configuration = Configuration
  { currentTerm :: Maybe Term
  , actions     :: [Action]
  , environment :: Environment
  }

initConfiguration :: Term -> Configuration
initConfiguration term = Configuration
  { currentTerm = Just term
  , actions = []
  , environment = []
  }

steps :: Configuration -> [Configuration]
steps = unfoldr (\conf -> (,) conf <$> step conf)

step :: Configuration -> Maybe Configuration
step conf@Configuration{..} =
  case currentTerm of
    Just (Loc 0) -> Just conf
      { currentTerm = Nothing }
    Just (Loc n) ->
      case environment of
        [] -> Nothing
        (_ : env) -> Just conf
          { currentTerm = Just (Loc (n - 1))
          , environment = env
          }
    Just (Dot t a) -> Just conf
      { currentTerm = Just t
      , actions = DotAction a : actions
      }
    Just (App t (a, u)) -> Just conf
      { currentTerm = Just t
      , actions = AppAction (a, (u, environment)) : actions
      }
    Just (Obj o) -> Just conf
      { currentTerm = Nothing
      , environment = Parent o HashMap.empty : environment
      }
    Nothing ->
      case actions of
        DotAction a : as ->
          case environment of
            Parent{..} : _env ->
              case original .? a of
                Nothing       -> Nothing    -- runtime error?
                Just VoidAttr ->
                  case HashMap.lookup a applications of
                    Nothing -> Nothing -- runtime error?
                    Just (u, e') -> Just conf
                      { currentTerm = Just u
                      , actions     = as
                      , environment = e'
                      }
                Just (Attached u) -> Just conf
                  { currentTerm = Just u
                  , actions = as
                  }
            _ -> Nothing -- should never happen?
        AppAction (a, (u, e')) : as ->
          case environment of
            Parent{..} : env ->
              case original .? a of
                Nothing         -> Nothing -- runtime error?
                Just Attached{} -> Nothing -- runtime error?
                Just VoidAttr
                  | HashMap.member a applications -> Nothing -- runtime error?
                  | otherwise -> Just conf
                      { actions = as
                      , environment = Parent
                          { applications = HashMap.insert a (u, e') applications
                          , .. } : env
                      }
            _ -> Nothing -- should never happen?
        [] -> Nothing -- nothing left to do
