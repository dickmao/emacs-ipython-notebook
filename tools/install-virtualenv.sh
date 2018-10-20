#!/bin/bash

# Create virtualenvs for python{2,3} for Travis CI on OSX

set -x

WORKDIR=${HOME}/local

. tools/retry.sh

if [ "x$TRAVIS_OS_NAME" = "xosx" ]; then

    # Install some custom requirements on OS X
    # e.g. brew install pyenv-virtualenv

    case "${TOXENV}" in
        py27)
            virtualenv -p python2 $WORKDIR/py27
            ;;
        py35)
            virtualenv -p python3 $WORKDIR/py35
            ;;
    esac
fi
