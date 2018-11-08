# 16.04 based: mothur hangs (#42)
FROM fpco/stack-build:lts-8.6

# FROM ubuntu:17.04 # no /usr/bin/setarch x86_64 -R 
# FROM ubuntu:14.04 # No hashdeep
# FROM ubuntu:15.10 

MAINTAINER Ryan G. Scott <ryan.gl.scott@gmail.com>

RUN apt-get -y update && apt-get -y upgrade && \
    apt-get -y install hashdeep time 

# Only when starting from scratch with ubuntu:
# RUN apt-get -y install gcc netbase git zlib1g-dev \
#                haskell-stack ghc
# RUN stack --install-ghc --resolver=lts-6.35 setup

RUN head -c 10000 /dev/urandom > /etc/pregen_random

ADD ./bin/replace_urandom.sh /bin/replace_urandom.sh

# ENTRYPOINT ["/bin/replace_urandom.sh"]
