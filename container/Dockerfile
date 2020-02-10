FROM debian:testing-20200130-slim
RUN \
  apt-get update && \
  apt-get install --assume-yes libgmp-dev && \
  adduser --disabled-password --gecos '' leaf
USER leaf
WORKDIR /home/leaf
ADD . .
ENV monadoc_datadir /home/leaf/data
EXPOSE 8080
CMD /home/leaf/monadoc
