#!/usr/bin/env bash
set -euo pipefail

if [[ -z "${1:-}" ]]; then
  echo "Usage ./scripts/setup_graalvm.sh <java-major-version>"
  exit 1
fi

cd "$(dirname $0)/.."
java_major_version="$1"
root_dir="$(pwd)"

graalvm_version=$(cat ${root_dir}/graalvm.version)
graalvm_dir="$root_dir/.graalvm/$graalvm_version"

if [[ -f "$graalvm_dir/bin/gu" ]]; then
  echo "GraalVM already installed, skip setup step."
  exit 0
fi


case "$(uname -s)" in
  Linux*)     graalvm_os="linux-amd64" ;;
  Darwin*)    graalvm_os="darwin-amd64" ;;
  *)          echo "OS not supported" && exit 1
esac

graalvm_jvm_version="$java_major_version"
graalvm_url="https://github.com/graalvm/graalvm-ce-builds/releases/download/vm-${graalvm_version}/graalvm-ce-java${graalvm_jvm_version}-${graalvm_os}-${graalvm_version}.tar.gz"

mkdir -p "$graalvm_dir"
cd "$graalvm_dir"
echo "Download..."
curl -L -o graalvm.tar.gz "$graalvm_url"

echo "Unpack..."
tar xzf graalvm.tar.gz
mv graalvm-ce-* libexec

echo "Link..."
rm -rf bin

case "$(uname -s)" in
  Linux*)     graalvm_bin_dir="$graalvm_dir/libexec/bin" ;;
  Darwin*)    graalvm_bin_dir="$graalvm_dir/libexec/Contents/Home/bin" ;;
esac

ln -s "$graalvm_bin_dir" bin

echo "Install native-image..."
bin/gu install native-image

echo "GraalVM installed."
