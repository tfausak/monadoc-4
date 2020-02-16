FROM amazonlinux:2
WORKDIR /root/monadoc
ENV monadoc_datadir /root/monadoc/data
EXPOSE 8080
RUN \
  yum install --assumeyes gmp-devel && \
  yum clean all && \
  rm --recursive /var/cache/yum
ADD monadoc .
ARG COMMIT
ENV monadoc_commit $COMMIT
CMD /root/monadoc/monadoc
