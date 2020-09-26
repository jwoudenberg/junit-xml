#!/usr/bin/env bash

set -euxo pipefail

name=$(cat package.yaml | grep name: | awk '{print $2}')
version=$(cat package.yaml | grep version: | awk '{print $2}')
bundle="$name-$version.tar.gz"

# check changelog contains an entry for this version
grep "^# $version$" < CHANGELOG.md

# check copyright year is current year
grep "^copyright: $(date +'%Y')" < package.yaml
grep "Copyright (c) $(date +'%Y')" < LICENSE

# check github release tag exists
git fetch --tags
git tag -l --points-at HEAD | grep "^$version$"

hpack
cabal sdist -o - > "$bundle"
cabal upload --publish "$bundle"
cabal upload -d --publish
