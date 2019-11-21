#!/usr/bin/env bash
CONFIG="Release"
BUILD_DIR="builds"
SRTM_CACHE=${PWD}/samples/cache

rm -rf ${BUILD_DIR}

dotnet build --configuration ${CONFIG} --verbosity minimal --no-incremental
dotnet test --configuration ${CONFIG} --verbosity minimal \
	--filter Category!=acceptance
dotnet publish Demeton.Console -c ${CONFIG} -r win10-x64 \
    --output ${PWD}/${BUILD_DIR}/Demeton-win10-x64
