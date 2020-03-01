#! /usr/bin/env sh
set -o errexit -o verbose

GHCUP_URL='https://gitlab.haskell.org/haskell/ghcup/uploads/9f278318810f652a59b1784c2295fa91/ghcup'
GHC_VERSION='8.8.2'
CABAL_VERSION='3.0.0.0'

yum install --assumeyes \
  gcc \
  gmp-devel \
  gzip \
  make \
  ncurses-devel \
  perl \
  postgresql-devel \
  tar \
  xz \
  zlib-devel

if test ! -f /usr/local/bin/ghcup
then
  curl --output /usr/local/bin/ghcup "$GHCUP_URL"
  chmod +x /usr/local/bin/ghcup
fi

if test ! -f ~/.ghcup/bin/ghc
then
  ghcup install "$GHC_VERSION"
  ghcup set "$GHC_VERSION"
fi

if test ! -f ~/.ghcup/bin/cabal
then
  ghcup install-cabal "$CABAL_VERSION"
fi
