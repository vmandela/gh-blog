{-# LANGUAGE OverloadedStrings          #-}
module Hakyll.Web.Bulma.Tags
  ( tagsFieldBulma
  )
where

import           Text.Blaze.Html                ( toHtml
                                                , toValue
                                                , (!)
                                                )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A
import           Hakyll
import           Data.List                      ( intersperse
                                                , sortBy
                                                )
import           Data.Text                      ( pack )

tagsFieldBulma :: String -> (String -> Tags -> Context a)
tagsFieldBulma x =
  tagsFieldWith getTags (simpleRenderLink x) (mconcat . intersperse " ")

--------------------------------------------------------------------------------

-- | Render one tag link

simpleRenderLink :: String -> String -> (Maybe FilePath) -> Maybe H.Html
simpleRenderLink x _ Nothing = Nothing
simpleRenderLink x tag (Just filePath) =
  Just
    $ H.a
    ! A.href (toValue $ toUrl filePath)
    ! A.class_ (H.textValue $ pack x)
    $ toHtml tag
