---
title: Adding anchor links to the site
tags: pandoc, blog setup, haskell
---

Anchor links are cool.
They make it easy to get the link to a specific section in webpage to share with others.
Just hover over a heading and get the clickable link with the URL of the heading.
This page lists my experiments with adding Anchor links to this blog.

## Using AnchorJS

I used [AnchorJS](https://www.bryanbraun.com/anchorjs/) in my first attempt to
add anchor links. It was very easy to integrate.

1. Include
   [`anchor.js`](https://github.com/bryanbraun/anchorjs/blob/master/anchor.min.js)
   script in the html page.

    ~~~{.html}
    <script src="/js/jquery-3.4.1.min.js"></script>
    <script src="js/apply-styles.js"></script>
    <!-- For anchor links -->
    <script src="js/anchor-4.2.0.min.js"></script>
    ~~~

2. and call `anchors.add()` in the `$(document).ready` call back. This was the
   location I used for the other style updates.

    ~~~{.js}
    /* apply-styles.js */
     $(document).ready(function() {
       anchors.add();
     }
    ~~~

3. [There is no Step 3](https://www.youtube.com/watch?v=YHzM4avGrKI)


### Moving away from AnchorJS

AnchorJS worked splendidly for my blog (for the audience of 1 &#128515;). However
the anchorJS website includes an explicit warning against calling
`anchors.add()` in the `$(document).ready` call back.

<https://www.bryanbraun.com/anchorjs/#dont-run-it-too-late>

> Don't add anchors on later events, like `$(document).ready()` or
> `window.onload` as some browsers will attempt to jump to your ID before
> AnchorJS can add it to the page. For more details, see
> [github issue #69](https://github.com/bryanbraun/anchorjs/issues/69#issuecomment-255503575).

This got me thinking. As my blog is static website, I figured I can
add the anchors when building the website and remove the runtime
component.

## Using Pandoc for Anchor Links

This blog is built using [Hakyll](http://jaspervdj.be/hakyll)
which uses [Pandoc](https://pandoc.org/) to convert from
Markdown to HTML.
I implemented a Pandoc filter to add anchors to headers

~~~{.hs}
-- Create an anchor based on the string
-- add anchorjs-link class for CSS manipulations
-- Use "#" as the display for the anchor
-- Append linkId to "#" to create the link target
addAnchor :: String -> Inline
addAnchor linkId = Link ("", ["anchorjs-link"], []) [Str " #"] ("#" ++ linkId, "")

-- Add a anchor link to each html header
-- Leave everything else as is
modHeader :: Block -> Block

-- We are matching against the header pattern from pandoc-types
-- We take the linkId and use it to add anchor links
--         Header Int Attr [Inline]
modHeader (Header n y@(linkId, _, _) xs) = (Header n y (xs ++ [(addAnchor linkId)]))
-- fallback for all others
modHeader x                              = x

-- Walk the Pandoc AST and apply modHeader on each node
anchorLinks :: Pandoc -> Pandoc
anchorLinks = walk modHeader
~~~

This script also needs CSS changes to hide the anchors by default
and reveal them on hover.

~~~{.css}
/* Make the anchor transparent by default */
.anchorjs-link {
    opacity: 0;
    text-decoration: none;
    -webkit-font-smoothing: antialiased;
    -moz-osx-font-smoothing: grayscale;
}

/* Make the anchor opaque on hover */
*:hover > .anchorjs-link,
.anchorjs-link:focus  {
    opacity: 1;
}
~~~

Here are the links to [haskell source](/static_root/AnchorJS.hs)
and [css file](/css/custom.css).

## References

1. AnchorJS - <https://www.bryanbraun.com/anchorjs/>
1. Use of Mouse-Over Paragraph Marker in Headlines for Permalink - <https://ux.stackexchange.com/questions/36304/use-of-mouse-over-paragraph-marker-in-headlines-for-permalink>
