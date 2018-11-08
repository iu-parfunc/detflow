#!/bin/bash
set -xe

stack exec -- detflow HelloFiles.hs -i in -o out
