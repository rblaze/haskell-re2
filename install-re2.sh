#!/bin/sh
set -ex

TARBALL=`curl https://api.github.com/repos/google/re2/releases/latest | jq -r ".tarball_url"`
if [ ${TARBALL} = "null" ] ; then exit 1 ; fi
curl -L -o source.tar.gz ${TARBALL}
tar -xzvf source.tar.gz
mv google-re2-* google-re2
cd google-re2 && make
