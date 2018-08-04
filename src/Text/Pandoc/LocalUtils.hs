{- |
   Module      : Text.Pandoc.LocalUtils
   Copyright   : Copyright (C) 2022 Venkateswara Rao Mandela
   License     : MIT

  Strip or add slash from links as required.

-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
module Text.Pandoc.LocalUtils
  ( stripSlash, addSlash
  )
where

import qualified Data.Text as T

class StringUtils a where
  stripSlash :: a -> a
  addSlash :: a -> a

instance StringUtils String where
  stripSlash :: String -> String
  stripSlash (x:xs)
    | '/' == x = xs
    | otherwise = [x] ++ xs
  stripSlash _ = []

  addSlash :: String -> String
  addSlash (x : xs) | '/' == x  = x : xs
                    | otherwise = '/' : (x : xs)
  addSlash _ = []

instance StringUtils T.Text where
  stripSlash :: T.Text -> T.Text
  stripSlash x = T.pack (stripSlash $ T.unpack x)

  addSlash :: T.Text -> T.Text
  addSlash x = T.pack (addSlash $ T.unpack x)
