#! /usr/bin/env sh
checkout
mkdir -p "$STACK_ROOT" "$STACK_WORK"
cache_key="stack-$( checksum stack.yaml )-$( checksum monadoc.cabal )"
cache restore "$cache_key"
stack --allow-different-user --system-ghc build --pedantic
cache store "$cache_key" .stack
cp --recursive --verbose data container
cp --verbose "$( command -v monadoc )" container
cd container || exit
mkdir libs
ldd monadoc | awk '/=>/ { print $3 }' | xargs cp --target-directory libs --verbose
zip_file="monadoc-$SEMAPHORE_GIT_SHA.zip"
zip --recurse-paths --verbose "$zip_file" .
artifact push workflow --expire-in 1w "$zip_file"
