#! /usr/bin/env sh
set -o errexit -o verbose

export PATH="$HOME/.cabal/bin:$HOME/.ghcup/bin:$PATH"

ghcup --version
ghc --version
cabal --version

cabal update

eval "$( aws ecr get-login --no-include-email )"
ECR_URL="$( aws ecr describe-repositories \
  --output text \
  --query 'repositories[0].repositoryUri' \
  --repository-names monadoc )"
export DOCKER_TAG="$ECR_URL:$CODEBUILD_RESOLVED_SOURCE_VERSION"
