#!/bin/sh -ex
sudo apt-get update -qq
sudo apt-get --no-install-recommends install darcs libgtksourceview-3.0-dev
darcs get --lazy http://patch-tag.com/r/hamish/gtk2hs
cd gtk2hs/tools
cabal install

