#!/bin/bash

rm -fr node01
rm -fr node02
mkdir -p node01
mkdir -p node02
chmod 777 node01
chmod 777 node02
export TIMESTAMP=$(date --date="+10 seconds" +%s%N | cut -b1-13)
echo > node01/config.yaml "\
bifrost:
  big-bang:
    staker-count: 2
    local-staker-index: 0
    timestamp: $TIMESTAMP
    regtest-enabled: true
    stakes: [10000, 10000]
"
chmod 777 node01/config.yaml
echo $(pwd)
export CONTAINER_ID=$(docker run -d --name bifrost01 -p 9185:9085 -p 9184:9084 -p 9191:9091 -v $(pwd)/node01:/bifrost-staking:rw ghcr.io/topl/bifrost-node:2.0.0-beta3-24-7fd725a9 --  --config=/bifrost-staking/config.yaml --regtest)
export IP_CONTAINER=$(docker network inspect bridge | jq  ".[0].Containers.\"$CONTAINER_ID\".IPv4Address" | sed  's:"::g' | sed -n 's:\(.*\)/.*:\1:p')
echo "IP_CONTAINER: $IP_CONTAINER"
echo > node02/config.yaml "\
bifrost:
  big-bang:
    staker-count: 2
    local-staker-index: 1
    timestamp: $TIMESTAMP
    regtest-enabled: true
    stakes: [10000, 10000]
  p2p:
    known-peers: $IP_CONTAINER:9085
"
chmod 777 node02/config.yaml
docker run -d --name bifrost02 -p 9087:9085 -p 9086:9084 -p 9092:9091 -v $(pwd)/node02:/bifrost-staking ghcr.io/topl/bifrost-node:2.0.0-beta3-24-7fd725a9 --  --config  /bifrost-staking/config.yaml --regtest
