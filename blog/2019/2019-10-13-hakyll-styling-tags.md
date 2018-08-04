---
title: Styling Hakyll Tags
tags: blog setup, haskell, hakyll
---

This blog is generated using Hakyll. Hakyll provides a way to annotate posts with tags and generate a list of pages with the same tag. I followed the excellent guide from <https://javran.github.io/posts/2014-03-01-add-tags-to-your-hakyll-blog.html> and added
the tags feature to the blog. The html produced for the tags is quite bare 

~~~{.html}
<a href="../../tags/blog%20setup.html">blog setup</a>,
<a href="../../tags/haskell.html">haskell</a>,
<a href="../../tags/hakyll.html">hakyll</a>
~~~

<a href="../../tags/blog%20setup.html">blog setup</a>, <a href="../../tags/haskell.html">haskell</a>, <a href="../../tags/hakyll.html">hakyll</a>

and I wanted to style it using bulma "is-link" and "tag" attributes.

~~~{.html}
<a href="../../tags/blog%20setup.html" class="tag is-link">blog setup</a>
<a href="../../tags/haskell.html" class="tag is-link">haskell</a>
<a href="../../tags/hakyll.html" class="tag is-link">hakyll</a>
~~~

<a href="../../tags/blog%20setup.html" class="tag is-link">blog setup</a> <a href="../../tags/haskell.html" class="tag is-link">haskell</a> <a href="../../tags/hakyll.html" class="tag is-link">hakyll</a>

While this can be done with jquery after the page is loaded, I wanted to do the styling during site generation. I took this as an opportunity to play with Hakyll.

The html for the tags is added to the Hakyll post context by the [`tagsField`](https://hackage.haskell.org/package/hakyll-4.13.0.1/docs/Hakyll-Web-Tags.html#v:tagsField) function.

~~~{.haskell}
postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags `mappend` postCtx
~~~

Hakyll also offers a [`tagsFieldWith`](https://hackage.haskell.org/package/hakyll-4.13.0.1/docs/Hakyll-Web-Tags.html#v:tagsFieldWith) function where the HTML generation can be customized. Below is the code I ended up with. 

~~~{.haskell}
postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsFieldBulma "tags" tags `mappend` postCtx


tagsFieldBulma :: String -> Tags -> Context a
tagsFieldBulma =
  tagsFieldWith getTags simpleRenderLinkBulma (mconcat . intersperse " ")

simpleRenderLinkBulma :: String -> (Maybe FilePath) -> Maybe H.Html
simpleRenderLinkBulma _ Nothing = Nothing
simpleRenderLinkBulma tag (Just filePath) =
  Just $ H.a ! A.href (toValue $ toUrl filePath) ! A.class_ "tag is-link" $ toHtml tag
~~~

This snippet custmoizes the  [`simpleRenderLink`](https://hackage.haskell.org/package/hakyll-4.13.0.1/docs/src/Hakyll.Web.Tags.html#simpleRenderLink) function to add `tag is-link` attributes to the tags. This function is plugged into `tagsFieldBulma` which gets plugged into `postCtxWithTags`.
The link to full source is here [Tags.hs](/static_root/Tags.hs).
