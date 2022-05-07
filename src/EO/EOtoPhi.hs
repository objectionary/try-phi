module EO.EOtoPhi where

import EOParser as EO (K, Term)
import Phi.Minimal.Model as Min

toMinimalTerm :: K EO.Term -> Min.Term
toMinimalTerm t = undefined