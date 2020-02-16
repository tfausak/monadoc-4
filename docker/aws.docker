FROM amazonlinux:2.0.20191217.0
RUN \
  yum update --assumeyes && \
  yum install --assumeyes awscli groff && \
  yum clean all && \
  rm --recursive /var/cache/yum
