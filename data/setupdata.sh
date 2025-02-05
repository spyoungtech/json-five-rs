#!/usr/bin/env bash

# add some data files that are too big to commit

curl https://raw.githubusercontent.com/json-iterator/test-data/refs/heads/master/large-file.json > data/big.json
curl https://gist.githubusercontent.com/spyoungtech/30439ffb42ec5b07e49e05f29951bb2d/raw/1f207dffed2432b609dd5ad3863d44cfae551c4b/problem.json > data/medium-ascii.json
