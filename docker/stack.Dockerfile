FROM amazonlinux:2.0.20191217.0
RUN \
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
    zlib-devel && \
  yum clean all && \
  rm --recursive /var/cache/yum && \
  curl \
    --location \
    --output stack.tar.gz \
    https://github.com/commercialhaskell/stack/releases/download/v2.1.3/stack-2.1.3-linux-x86_64-static.tar.gz && \
  tar \
    --extract \
    --file stack.tar.gz \
    --strip-components 1 \
    --wildcards '*/stack' && \
  rm stack.tar.gz && \
  mv stack /usr/local/bin
