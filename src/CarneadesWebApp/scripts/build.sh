#!/usr/bin/env bash

set -e

export LEIN_SNAPSHOTS_IN_RELEASE=y

lein sub install
lein deps
