#!/bin/bash
# Dockerfile "entrypoint".

mv /dev/urandom /dev/urandom_unused
ln -s /etc/pregen_random /dev/urandom

mv /dev/random /dev/random_unused
ln -s /etc/pregen_random /dev/random

# This seems to be the favored stack entrypoint?
if [[ $@ ]]; then
  /usr/local/sbin/pid1 $@
fi
