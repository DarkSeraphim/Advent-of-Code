#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "usage: $0 YEAR" >&2
  exit 1
fi

year="$1"
root="$(cd "$(dirname "$0")" && pwd)"
dest="$root/$year"

if [[ -e "$dest" ]]; then
  echo "error: $dest already exists" >&2
  exit 1
fi

cp -RP "$root/template" "$dest"

find "$dest" -type f -exec sed -i.bak "s/{{YEAR}}/$year/g" {} +
find "$dest" -name '*.bak' -delete

echo "Created the project for year $year"
