#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname $0)/.."
root_dir="$(pwd)"

graalvm_version_string=$(cat ${root_dir}/graalvm.version)
graalvm_dir="$root_dir/.graalvm/$graalvm_version_string"

IFS='-' read -ra versions_info <<< "$graalvm_version_string"
graalvm_version="${versions_info["0"]}"
graalvm_jvm_version="${versions_info["1"]}"

echo "GraalVM version: $graalvm_version"
echo "Java version: $graalvm_jvm_version"

if [[ -d "$graalvm_dir/libexec" ]]; then
  echo "GraalVM already installed, skip download step."
  cd "$graalvm_dir"
else

  case "$(uname -s)" in
    Linux*)     graalvm_os="linux-amd64" ;;
    Darwin*)    graalvm_os="darwin-amd64" ;;
    *)          echo "OS not supported" && exit 1
  esac

  graalvm_url="https://github.com/graalvm/graalvm-ce-builds/releases/download/vm-${graalvm_version}/graalvm-ce-${graalvm_jvm_version}-${graalvm_os}-${graalvm_version}.tar.gz"

  mkdir -p "$graalvm_dir"
  cd "$graalvm_dir"
  echo "Download..."
  curl -L -o graalvm.tar.gz "$graalvm_url"

  echo "Unpack..."
  tar xzf graalvm.tar.gz
  mv graalvm-ce-* libexec

fi


echo "Link..."
cd ..
rm -rf current

case "$(uname -s)" in
  Linux*)     graalvm_java_home="$graalvm_dir/libexec" ;;
  Darwin*)    graalvm_java_home="$graalvm_dir/libexec/Contents/Home" ;;
esac

ln -s "$graalvm_java_home" current

echo "Install native-image..."
current/bin/gu install native-image

echo "GraalVM installed."
