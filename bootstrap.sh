#!/usr/bin/env bash

apt-get update

curl -sL https://deb.nodesource.com/setup_9.x | sudo -E bash -
sudo apt-get install -y nodejs

sudo chown -R $(whoami) /usr/lib/node_modules/

sudo npm install --unsafe-perm -g elm
