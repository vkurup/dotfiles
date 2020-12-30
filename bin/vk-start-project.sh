#!/bin/bash
set -e

if [ "$1" == "" ]; then
    echo "usage: $0 PROJECT_NAME"
    exit 99
fi

mkdir -p ~/dev/$1
rm -rf ~/dev/$1/.direnv
cp ~/.config/direnv/envrc ~/dev/$1/.envrc

echo "Done: ~/dev/$1/"
