#!/bin/bash

# TODO: switch over to using `detflow` so that detmake doesn't have
# any "trusted" IO monad code:
stack exec -- detmake
