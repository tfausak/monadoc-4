#! /usr/bin/env bash

directory="$( pwd )"
stackRoot="$directory/.stack/root"
stackWork='.stack/work'
options=(
  '--env' "STACK_ROOT=$stackRoot"
  '--env' "STACK_WORK=$stackWork"
  '--interactive'
  '--rm'
  '--volume' "$directory:$directory"
  '--workdir' "$directory"
)

if test -z "$NO_TTY"
then
  options+=('--tty')
fi

if test -n "$PUBLISH"
then
  options+=('--publish' '8080:8080')
fi

mkdir --parents "$stackRoot" "$stackWork"

exec docker run "${options[@]}" taylorfausak/stack:2.1.3 \
  stack --allow-different-user "$@"
