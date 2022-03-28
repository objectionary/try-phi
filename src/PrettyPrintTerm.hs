module PrettyPrintTerm where

import ToTerm(Term(..))

class PPTerm a where 
    pprint :: Int -> a -> String

