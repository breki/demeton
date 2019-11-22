#!/usr/bin/env bash

# Release script run as part of a release GitHub action.
# The script compiles the code, runs the tests and then packages the binaries
# for Windows and Linux distribution. 

if [[ "$1" == "" ]]; then
    echo "ERROR: Missing release version number"
    echo ""
    echo "USAGE: ./release.sh <version number>"
    exit 1
fi

VERSION=$1

CONFIG="Release"
BUILD_DIR="builds"
SRTM_CACHE=./samples/cache

rm -rf ${BUILD_DIR}

dotnet build --configuration ${CONFIG} --verbosity minimal --no-incremental
dotnet test --configuration ${CONFIG} --verbosity minimal \
	--filter Category!=acceptance
	
# publish the binaries for various distributions
dotnet publish Demeton.Console -c ${CONFIG} -r win10-x64 --self-contained false \
    --output ${BUILD_DIR}/Demeton-win10-x64
dotnet publish Demeton.Console -c ${CONFIG} -r win10-x64 --self-contained true \
    --output ${BUILD_DIR}/Demeton-win10-x64-full
dotnet publish Demeton.Console -c ${CONFIG} -r linux-x64 --self-contained true \
    --output ${BUILD_DIR}/Demeton-linux-x64-full

# zip the binaries
cd ${BUILD_DIR}
zip -qq -r Demeton-win10-x64-${VERSION}.zip ./Demeton-win10-x64
zip -qq -r Demeton-win10-x64-full-${VERSION}.zip ./Demeton-win10-x64-full
zip -qq -r Demeton-linux-x64-full-${VERSION}.zip ./Demeton-linux-x64-full
cd -
