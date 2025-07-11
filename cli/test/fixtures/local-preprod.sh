#!/usr/bin/env bash

export ANTI_WALLET_FILE=tmp/test.json
export ANTI_TEST_REQUESTER_WALLET=tmp/test.json
export ANTI_TEST_ORACLE_WALLET=tmp/test.json
export ANTI_MPFS_HOST=http://localhost:3000
export ANTI_CONFIG_FILE=test/fixtures/anti-config.json
export ANTI_WAIT=180
address=$(anti wallet info | jq -r '.result.address')
echo "Funding address: $address"
curl -X 'POST' \
  'http://localhost:10000/local-cluster/api/addresses/topup' \
  -H 'accept: */*' \
  -H 'Content-Type: application/json' \
  -d '{
  "address": "'"$address"'",
  "adaAmount": 10000
}' | jq -r '.message'