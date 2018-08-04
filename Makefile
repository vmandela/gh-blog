all: site

site: site.hs
	cabal build gh-blog
	cabal run gh-blog -- build

rebuild: site.hs
	cabal build gh-blog
	cabal run gh-blog -- rebuild

site_clean :
	cabal --enable-nix clean

clean: site_clean
