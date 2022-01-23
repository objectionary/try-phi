{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

module Phi.Minimal.Machine.CallByName where

import           Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import           Data.List                  (unfoldr)

import Phi.Minimal.Model

type Environment = [Parent]

data Parent =
  Parent
    { original     :: Object Term
    , applications :: InsOrdHashMap Attr (Term, Environment)
    }

data Action
  = DotAction Attr
  | AppAction (Attr, (Term, Environment))

data Configuration =
  Configuration
    { currentTerm :: Maybe Term
    , actions     :: [Action]
    , environment :: Environment
    }

initConfiguration :: Term -> Configuration
initConfiguration term =
  Configuration {currentTerm = Just term, actions = [], environment = []}

-- | Note: this does not work with open argument terms.
-- IDK what's 'open argument terms'
fromParent :: Parent -> Term
fromParent Parent {..} =
  Obj $
  Object $
  InsOrdHashMap.unionWith
    (flip const)
    (getObject original)
    (Attached . incLocators . from <$> applications)
  where
    from (u, e) = withParents u e

fromAction :: Term -> Action -> Term
fromAction t =
  \case
    DotAction a -> Dot t a
    AppAction (a, (u, e)) -> App t (a, withParents u e)

fromActions :: Term -> [Action] -> Term
fromActions = foldl fromAction

withParents :: Term -> [Parent] -> Term
withParents = foldl $ \t parent -> substituteLocator (0, fromParent parent) t

fromParents1 :: [Parent] -> Term
fromParents1 [] = error "invalid Configuration"
fromParents1 (Parent term apps:parents) =
  case withParents (Obj term) parents of
    Obj term' -> fromParent (Parent term' apps)
    _         -> error "impossible"

fromConfiguration :: Configuration -> Term
fromConfiguration Configuration {..} =
  case currentTerm of
    Nothing -> fromActions (fromParents1 environment) actions
    Just t  -> withParents (fromActions t actions) environment

stepThrough :: Configuration -> Configuration
stepThrough = last . steps

stepThroughN :: Int -> Configuration -> Configuration
stepThroughN maxSteps = last . take maxSteps . steps

steps :: Configuration -> [Configuration]
steps c = c : unfoldr (\conf -> dup <$> step conf) c --IDK why use dup
  where
    dup x = (x, x)

step :: Configuration -> Maybe Configuration
step conf@Configuration {..} =
  case currentTerm of
    Just (Loc 0)
      | null environment -> Nothing
      | otherwise -> Just conf {currentTerm = Nothing}
    Just (Loc n) ->
      case environment of
        [] -> Nothing
        (_:env) ->
          Just conf {currentTerm = Just (Loc (n - 1)), environment = env}
    Just (Dot t a) ->
      Just conf {currentTerm = Just t, actions = DotAction a : actions}
    Just (App t (a, u)) ->
      Just
        conf
          { currentTerm = Just t
          , actions = AppAction (a, (u, environment)) : actions
          }
    Just (Obj o) ->
      Just
        conf
          { currentTerm = Nothing
          , environment = Parent o InsOrdHashMap.empty : environment
          }
    Nothing ->
      case actions of
        DotAction a:as ->
          case environment of
            Parent {..}:_env ->
              case original .? a of
                Nothing -> Nothing -- runtime error?
                Just VoidAttr ->
                  case InsOrdHashMap.lookup a applications of
                    Nothing -> Nothing -- runtime error?
                    Just (u, e') ->
                      Just
                        conf
                          {currentTerm = Just u, actions = as, environment = e'}
                Just (Attached u) ->
                  Just conf {currentTerm = Just u, actions = as}
            _ -> Nothing -- should never happen?
        AppAction (a, (u, e')):as ->
          case environment of
            Parent {..}:env ->
              case original .? a of
                Nothing -> Nothing -- runtime error?
                Just Attached {} -> Nothing -- runtime error?
                Just VoidAttr
                  | InsOrdHashMap.member a applications -> Nothing -- runtime error?
                  | otherwise ->
                    Just
                      conf
                        { actions = as
                        , environment =
                            Parent
                              { applications =
                                  InsOrdHashMap.insert a (u, e') applications
                              , ..
                              } :
                            env
                        }
            _ -> Nothing -- should never happen?
        [] -> Nothing -- nothing left to do
