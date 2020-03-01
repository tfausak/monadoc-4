#! /usr/bin/env sh
set -o errexit -o verbose

cabal test --jobs
cabal install --install-method copy --installdir docker
cp --recursive --verbose data docker
docker build \
  --build-arg "COMMIT=$CODEBUILD_RESOLVED_SOURCE_VERSION" \
  --tag "$DOCKER_TAG" \
  docker
