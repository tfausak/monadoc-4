#! /usr/bin/env sh
set -o errexit -o verbose

test "$CODEBUILD_BUILD_SUCCEEDING" = 1

if ! aws ecr describe-images \
  --image-ids "imageTag=$CODEBUILD_RESOLVED_SOURCE_VERSION" \
  --repository-name monadoc
then
  docker push "$DOCKER_TAG"
fi

printf \
  '{ "Parameters": { "Commit": "%s" } }\n' \
  "$CODEBUILD_RESOLVED_SOURCE_VERSION" |
  tee aws/configuration.json
