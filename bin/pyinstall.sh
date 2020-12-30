#!/bin/bash
# Install all versions specified in .python-version
set -ex

while read version; do
    pyenv install -s "$version"
done <.python-version
