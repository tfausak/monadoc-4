FROM amazonlinux:2
RUN \
  yum install --assumeyes gmp-devel && \
  yum clean all && \
  rm --recursive /var/cache/yum
WORKDIR /root/monadoc
ADD . .
ENV monadoc_datadir /root/monadoc/data
EXPOSE 8080
ARG COMMIT
ENV monadoc_commit $COMMIT
CMD /home/monadoc/monadoc
