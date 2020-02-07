#! /usr/bin/env sh
checkout
mkdir -p "$STACK_ROOT" "$STACK_WORK"
cache_key="stack-$( checksum stack.yaml )"
cache restore "$cache_key"
stack --allow-different-user --system-ghc setup
cache store "$cache_key" .stack
