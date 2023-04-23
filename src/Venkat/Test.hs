{-# LANGUAGE QuasiQuotes #-}
import           Text.Pandoc as Pandoc
import           Text.Pandoc.Walk
import           Text.Pandoc.Definition
import qualified Data.Text                     as T
import           Hakyll
import           Data.Either
import           Data.Char                      ( toUpper )
import           Text.RawString.QQ
import           Text.Pandoc.AnchorJS

fname = "templates/bootstrap.html"
-- tempStr Pandoc.Template
tempStr = T.pack "$toc$\n$body$"
tocTemplate =
        either error id $ either (error . show) id $
        Pandoc.runPure $ Pandoc.runWithDefaultPartials $
        Pandoc.compileTemplate fname tempStr

inp :: String
inp = [r|Hello World

# Hello

How do you do ?

## Header 2

[Hello](My friend.)

### Header 3

Its been a long time

#### Header 4|]

ropt = defaultHakyllReaderOptions
wopt = defaultHakyllWriterOptions { writerTableOfContents = True
                                   , writerTemplate        = Just tocTemplate
                                  }
syntree = runPure (readMarkdown ropt $ T.pack inp)

out = case syntree of
  (Left  _) -> error "Syntax error"
  (Right p) -> runPure (writeHtml5String wopt p)


modSyntree = case syntree of
  (Left  _) -> error "Syntax error"
  (Right p) -> anchorLinks p


out2 = runPure (writeHtml5String wopt modSyntree)
