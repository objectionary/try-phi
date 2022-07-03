module Main where

import Lib ( startApp )
-- import Faker.Cannabis as FC
-- import Faker
-- import Faker.Name as FN
-- import qualified Faker.Cannabis as FC
-- import Faker.ChuckNorris as FCN
-- import qualified Faker.ChuckNorris as FCN
import Data.Text.IO as DT


main :: IO ()
main = do
    startApp
    -- return ()
    -- p <- generateWithSettings (setNonDeterministic defaultFakerSettings) FC.cannabinoids
    -- p <- generateWithSettings (setNonDeterministic defaultFakerSettings) FCN.fact
    -- DT.putStrLn p    
