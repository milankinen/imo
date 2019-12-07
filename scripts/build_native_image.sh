#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname $0)/.."

case "$(uname -s)" in
    Linux*)     binary_name="imo-linux-amd64" ;;
    Darwin*)    binary_name="imo-osx-amd64" ;;
    *)          echo "OS not supported" && exit 1
esac

mkdir -p target/native

output="$(pwd)/target/native/$binary_name"
echo "Build native image: $output"

.graalvm/bin/native-image \
  --report-unsupported-elements-at-runtime \
  --initialize-at-build-time \
  --no-fallback \
  -jar ./target/uberjar/imo.jar \
  -H:Name=${output}

echo "Built."
