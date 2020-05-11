#!/usr/bin/env bash

hpack
cabal sdist -o - > bundle.tar.gz
cabal upload --publish bundle.tar.gz
cabal upload -d --publish
