#/bin/sh
set -e

[ -z "$1" ] && (echo "Usage: $0 <file to compile>"; exit 1)

[ -f bin/main ] || make clean main

bin/main evaluate "$1"
