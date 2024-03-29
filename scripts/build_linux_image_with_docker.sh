#!/usr/bin/env bash
set -euo pipefail

cd $(dirname $0)/..

echo "Build docker image for Linux build..."
docker build -t imo-linux-build-image \
  --build-arg GRAALVM_VERSION="$(cat graalvm.version)" \
  -f scripts/Dockerfile.linux-build \
  ./scripts

echo "Run build with docker..."
mkdir -p target/native
docker run --rm  \
  -v $(pwd):/host \
  imo-linux-build-image \
  /bin/bash -c 'cd /host \
    && cp -r src scripts project.clj /work \
    && cd /work \
    && ./scripts/build_native_image.sh \
    && cp /work/target/native/imo-linux* /host/target/native/'

echo "Done."
