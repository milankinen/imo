#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname $0)/.."
root_dir="$(pwd)"
graalvm_java_home="$root_dir/.graalvm/current"

case "$(uname -s)" in
    Linux*)     binary_name="imo-linux-amd64" ;;
    Darwin*)    binary_name="imo-osx-amd64" ;;
    *)          echo "OS not supported" && exit 1
esac

echo "Build uberjar"
rm -rf target/uberjar*
JAVA_HOME=$graalvm_java_home lein uberjar

output="$(pwd)/target/native/$binary_name"
echo "Build native image: $output"
mkdir -p target/native

$graalvm_java_home/bin/native-image \
  --report-unsupported-elements-at-runtime \
  --initialize-at-build-time \
  --no-fallback \
  -jar ./target/uberjar/imo.jar \
  -H:Name=${output}

echo "Built."
