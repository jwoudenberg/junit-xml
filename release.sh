#!/usr/bin/env bash

set -euxo pipefail

function fail {
  echo "$1"
  exit 1
}

name=$(grep name: < package.yaml | awk '{print $2}')
version=$(grep version: < package.yaml | awk '{print $2}')
bundle="$name-$version.tar.gz"

hpack

# check repository does not contain uncomitted changes
if git status --porcelain | grep . ; then
  fail "Stash any changes before starting the release script."
fi

# check changelog contains an entry for this version
grep "^# $version$" < CHANGELOG.md

# check copyright year is current year
grep "^copyright: $(date +'%Y')" < package.yaml
grep "Copyright (c) $(date +'%Y')" < LICENSE

# check github release tag exists
git fetch --tags
git tag -l --points-at HEAD | grep "^$version$"

cabal sdist -o - > "$bundle"
cabal upload --publish "$bundle"
cabal upload -d --publish
