#!/usr/bin/env bash
CONFIG="Release"
BUILD_DIR="builds\Demeton.Console"
SRTM_CACHE=${PWD}/samples/cache

rm -rf ${BUILD_DIR}

rem dotnet clean --configuration ${CONFIG}
dotnet build --configuration ${CONFIG} --verbosity minimal --no-incremental
dotnet test --configuration ${CONFIG} --verbosity minimal \
	--filter Category!=acceptance
dotnet publish Demeton.Console -c ${CONFIG} --output ${PWD}/${BUILD_DIR}
