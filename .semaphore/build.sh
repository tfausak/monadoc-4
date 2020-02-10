#! /usr/bin/env sh
checkout
mkdir --parents .stack/root .stack/work
cache_key="stack-$( checksum stack.yaml )-$( checksum monadoc.cabal )"
cache restore "$cache_key"
tools/stack.sh build --copy-bins --local-bin-path container --pedantic
cache store "$cache_key" .stack
cp --recursive --verbose data container
cd container || exit
zip_file="monadoc-$SEMAPHORE_GIT_SHA.zip"
zip --recurse-paths --verbose "$zip_file" .
artifact push workflow --expire-in 1w "$zip_file"
