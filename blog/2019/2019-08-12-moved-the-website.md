---
title: Moved the site to vmandela.com
tags: blog setup
---

I have changed the URL for my site from <https://vmandela.github.io> to <https://vmandela.com>. Now I have recurring expense renewing
the personal domain. :-)

The change is simple as the process is well documented on the Github [here](https://help.github.com/en/articles/using-a-custom-domain-with-github-pages) and [here](https://help.github.com/en/articles/setting-up-an-apex-domain#configuring-a-records-with-your-dns-provider). The change is my first brush with the different types of DNS records. I had to setup

1. An `A` record - Pointing the domain vmandela.com to GitHub's IP addresses.
2. An `CNAME` record - Pointing "www.vmandela.com" to "vmandela.com".

There are over 10 types of records listed on my domain sellers page. One more thing to add to the reading list. 