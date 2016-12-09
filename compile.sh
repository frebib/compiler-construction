#/bin/sh
set -e

[ -z "$1" ] && (echo "Usage: $0 <file to compile>"; exit 1)

[ -f bin/main ] || make clean main

bin/main compile "$1" | gcc -x assembler - -o "$1.elf"
