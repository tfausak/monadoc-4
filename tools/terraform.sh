#! /usr/bin/env sh

directory="$( pwd )"

exec docker run \
  --env "AWS_ACCESS_KEY_ID=$AWS_ACCESS_KEY_ID" \
  --env "AWS_DEFAULT_REGION=$AWS_DEFAULT_REGION" \
  --env "AWS_SECRET_ACCESS_KEY=$AWS_SECRET_ACCESS_KEY" \
  --interactive \
  --rm \
  --tty \
  --volume "$directory:$directory" \
  --workdir "$directory" \
  hashicorp/terraform:0.12.20 "$@"
