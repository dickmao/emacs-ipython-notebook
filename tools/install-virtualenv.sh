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
    pyenv version

    case "${TOXENV}" in
        py27)
            pyenv virtualenv 2.7 py27
            ;;
        py35)
            pyenv virtualenv 3.5 py35
            ;;
    esac
fi
