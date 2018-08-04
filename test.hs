-- |

module Test where

import           Hakyll
import Text.Pandoc as Pandoc
import Data.Text (pack, Text)
import System.IO.Unsafe


fname = "templates/bootstrap.html"
tempStr = pack $ unsafePerformIO $ readFile fname

-- l :: PandocIO Text
-- l = Pandoc.getTemplate fname

-- m :: WithDefaultPartials PandocIO (Either String (Template Text))
-- m =  l >>= Pandoc.compileTemplate fname

-- o = Pandoc.runWithDefaultPartials m

-- p =  Pandoc.runIOorExplode o

-- l = Pandoc.compileTemplate fname o


-- n = Pandoc.runPure $ m

-- tocTemplate =
--         either error id $ either (error . show) id $
--         Pandoc.runPure $ Pandoc.runWithDefaultPartials $
--         Pandoc.compileTemplate "" tempStr
