#!/usr/bin/env bash

# Downloads a couple of SRTM tiles from Demeton's GitHub repository to the
# local machine.

GITHUB_CACHE_URL="https://github.com/breki/demeton/raw/master/samples/cache/"
LOCAL_CACHE="cache/"

mkdir -p ${LOCAL_CACHE}0
cd ${LOCAL_CACHE}0
wget ${GITHUB_CACHE_URL}0/N46E013.png
wget ${GITHUB_CACHE_URL}0/N46E014.png
cd -