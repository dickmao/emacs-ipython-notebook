#!/bin/bash

# Create virtualenvs for python{2,3} for Travis CI on OSX

set -x

WORKDIR=${HOME}/local

. tools/retry.sh

if [ "x$TRAVIS_OS_NAME" = "xosx" ]; then

    brew update
    brew list python &>/dev/null || brew install python
    brew list python3 &>/dev/null || brew install python3
    brew install pyenv-virtualenv

    case "${TOXENV}" in
        py27)
            virtualenv -p python $WORKDIR/py27
            ;;
        py35)
            virtualenv -p python3 $WORKDIR/py35
            ;;
    esac
fi
