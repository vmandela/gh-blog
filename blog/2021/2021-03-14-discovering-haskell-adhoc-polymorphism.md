---
title: Discovering Haskell Adhoc Polymorphism
tags: haskell, hakyll, blog setup, pandoc
---

This blog is built with Hakyll. Hakyll uses Pandoc for markdown to html
conversion. The blog breaks approximately once an year with a change to the
underlying [pandoc-types](https://github.com/jgm/pandoc-types).

One of the past breaking changes was the conversion basic text data type from
`[Char]` to `Data.Text`. The change broke some of the filters I used in the
blog. One of the filters was a function to strip the initial `/` from any
resources used in the blog. Hakyll uses URLs with slash at the beginning '/' for
local images/resources. I used this filter to fixup the link to images to relative
links before converting them into Word document or PDF format.


~~~{.haskell}
stripSlash :: String -> String
stripSlash (x:xs)
  | '/' == x = xs
  | otherwise = [x] ++ xs
stripSlash _ = []
~~~

I was looking for a way to modify the `stripSlash` function to support both
`[Char]` and `Data.Text` data type. Thats how I discovered Haskells ad-hoc
polymorphism. This required defining a typeclass and creating an instance of it
for each data type I needed to support.

~~~{.haskell}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
import qualified Data.Text as T

class StringUtils a where
  stripSlash :: a -> a

instance StringUtils String where
  stripSlash :: String -> String
  stripSlash (x:xs)
    | '/' == x = xs
    | otherwise = [x] ++ xs
  stripSlash _ = []

instance StringUtils T.Text where
  stripSlash :: T.Text -> T.Text
  stripSlash x = T.pack (stripSlash $ T.unpack x)
~~~

Here are a couple of links that helped me.

1. <https://stackoverflow.com/questions/12430660/creating-polymorphic-functions-in-haskell>
2. <https://en.wikibooks.org/wiki/Haskell/Polymorphism#Ad-hoc_polymorphism>


I am one of todays lucky 10000. :-)

![Lucky 10000](https://imgs.xkcd.com/comics/ten_thousand.png)


