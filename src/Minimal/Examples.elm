module Minimal.Examples exposing (..)

import Minimal.Syntax exposing (Term(..))
import Minimal.Parser exposing (parse)

-- Examples from paper

getTerm : String -> Term 
getTerm s =
    case parse s of
        Ok term -> term
        _ -> Locator 0

-- lines 6-10
ex1 : Term
ex1 = getTerm "[book3 ->[\n isbn-> ?,\n title->^.ObjectThinking,\n price->^.memory,\n setprice->[p->?,@->^.price.write(t->$.p)]\n ]]"
    