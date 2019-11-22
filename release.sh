#!/usr/bin/env bash

# Release script run as part of a release GitHub action.
# The script compiles the code, runs the tests and then packages the binaries
# for Windows and Linux distribution. 

CONFIG="Release"
BUILD_DIR="builds"
SRTM_CACHE=./samples/cache

rm -rf ${BUILD_DIR}

dotnet build --configuration ${CONFIG} --verbosity minimal --no-incremental
dotnet test --configuration ${CONFIG} --verbosity minimal \
	--filter Category!=acceptance
	
# publish the binaries for various distributions
TESTS_DIR=${BUILD_DIR}/Demeton-test

dotnet publish Demeton.Console -c ${CONFIG} --output ${TESTS_DIR}
dotnet publish Demeton.Console -c ${CONFIG} -r win10-x64 --self-contained false \
    --output ${BUILD_DIR}/Demeton-win10-x64
dotnet publish Demeton.Console -c ${CONFIG} -r win10-x64 --self-contained true \
    --output ${BUILD_DIR}/Demeton-win10-x64-full
dotnet publish Demeton.Console -c ${CONFIG} -r linux-x64 --self-contained true \
    --output ${BUILD_DIR}/Demeton-linux-x64-full

# test: run the Demeton console to generate a sample image
cd ${TESTS_DIR}
./Demeton.Console shade 13.49437,46.159668,14.236633,46.543914 \
    --map-scale 500000 --tile-size 5000 --local-cache-dir ../../samples/cache
   
# ensure the test image is there
TEST_OUTPUT_IMAGE=${TESTS_DIR}/output/shading-0-0.png
if test -f "${TEST_OUTPUT_IMAGE}"; then
    echo "ERROR: Expected output image ${TEST_OUTPUT_IMAGE} is missing."
    exit 1
fi
cd -

# zip the binaries
cd ${BUILD_DIR}
zip -qq -r Demeton-win10-x64.zip ./Demeton-win10-x64
zip -qq -r Demeton-win10-x64-full.zip ./Demeton-win10-x64-full
zip -qq -r Demeton-linux-x64-full.zip ./Demeton-linux-x64-full
cd -
