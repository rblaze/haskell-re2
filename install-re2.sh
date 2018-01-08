#!/bin/sh
set -ex

TARBALL=`curl -s https://api.github.com/repos/google/re2/releases/latest | jq -r ".tarball_url"`
curl -L -o source.tar.gz ${TARBALL}
tar -xzvf source.tar.gz
mv google-re2-* google-re2
cd google-re2 && make
