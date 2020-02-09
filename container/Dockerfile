FROM debian:stretch-slim
RUN apt-get update && apt-get upgrade --assume-yes
RUN adduser --disabled-password --gecos '' monadoc
USER monadoc
WORKDIR /home/monadoc
ADD . .
ENV LD_LIBRARY_PATH /home/monadoc/libs
ENV monadoc_datadir /home/monadoc/data
EXPOSE 8080
CMD /home/monadoc/monadoc
