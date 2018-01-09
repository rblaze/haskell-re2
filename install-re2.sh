#!/bin/sh
set -ex

# GitHub API often returns 403 Forbidden
#TARBALL=`curl -v https://api.github.com/repos/google/re2/releases/latest | jq -r ".tarball_url"`
#if [ ${TARBALL} = "null" ] ; then exit 1 ; fi
TARBALL=https://github.com/google/re2/archive/2018-01-01.tar.gz
curl -L -o source.tar.gz ${TARBALL}
tar -xzvf source.tar.gz
#mv google-re2-* google-re2
mv re2-2018-01-01 google-re2
cd google-re2 && make
