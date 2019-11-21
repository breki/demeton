#!/usr/bin/env bash
CONFIG="Release"
BUILD_DIR="builds"
SRTM_CACHE=./samples/cache

rm -rf ${BUILD_DIR}

dotnet build --configuration ${CONFIG} --verbosity minimal --no-incremental
dotnet test --configuration ${CONFIG} --verbosity minimal \
	--filter Category!=acceptance
dotnet publish Demeton.Console -c ${CONFIG} -r win10-x64 --self-contained false \
    --output ${BUILD_DIR}/Demeton-win10-x64
dotnet publish Demeton.Console -c ${CONFIG} -r win10-x64 --self-contained true \
    --output ${BUILD_DIR}/Demeton-win10-x64-full
dotnet publish Demeton.Console -c ${CONFIG} -r linux-x64 --self-contained true \
    --output ${BUILD_DIR}/Demeton-linux-x64-full

cd ${BUILD_DIR}
zip -qq -r Demeton-win10-x64.zip ./Demeton-win10-x64
zip -qq -r Demeton-win10-x64-full.zip ./Demeton-win10-x64-full
zip -qq -r Demeton-linux-x64-full.zip ./Demeton-linux-x64-full
cd -
