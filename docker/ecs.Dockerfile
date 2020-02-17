FROM amazonlinux:2.0.20191217.0
WORKDIR /root/monadoc
ENV monadoc_datadir /root/monadoc/data
EXPOSE 8080
RUN \
  yum update --assumeyes && \
  yum install --assumeyes gmp-devel postgresql-devel && \
  yum clean all && \
  rm --recursive /var/cache/yum
ADD . .
ARG COMMIT
ENV monadoc_commit $COMMIT
CMD /root/monadoc/monadoc
