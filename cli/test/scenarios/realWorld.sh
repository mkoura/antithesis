#!/usr/bin/env bash

set -euo pipefail

# shellcheck disable=SC1091
source "$(dirname "$0")/lib.sh"

export ANTI_WAIT=240
unset ANTI_TOKEN_ID
# check the anti command is available
if ! command -v anti &> /dev/null; then
    echo "anti command not found, please make it available in your PATH"
    exit 1
fi
if [ -z "${ANTI_MPFS_HOST:-}" ]; then
    echo "Please set ANTI_MPFS_HOST environment variable, this is the host of the anti MPFS server"
    exit 1
fi
if [ -z "${ANTI_TEST_ORACLE_WALLET:-}" ]; then
    echo "Please set ANTI_TEST_ORACLE_WALLET environment variable"
    exit 1
fi
if [ -z "${ANTI_TEST_AGENT_WALLET:-}" ]; then
    echo "Please set ANTI_TEST_AGENT_WALLET environment variable"
    exit 1
fi
if [ -z "${ANTI_TEST_REQUESTER_WALLET:-}" ]; then
    echo "Please set ANTI_TEST_REQUESTER_WALLET environment variable"
    exit 1
fi
if [ -z "${ANTI_SSH_FILE:-}" ]; then
    echo "Please set ANTI_SSH_FILE environment variable, this is an encrypted SSH private key of 'cfhal'. \
        'cfhal' github user is registered on github. Get his key from 1 password vault.\
        https://start.1password.com/open/i?a=TYQQQLKUDBAFVHQ4P7XKFCUVYM&v=fhipthmhnufti4q2kky6d7336u&i=sjjrwnsdec5ajb4adeia2uixfy&h=cardanofoundation.1password.com"
    exit 1
fi
if [ -z "${ANTI_SSH_PASSWORD:-}" ]; then
    echo "Please set ANTI_SSH_PASSWORD environment variable, this is the passphrase for the encrypted SSH private key \
        available in 1password vault at the same record."
    exit 1
fi
if [ -z "${GITHUB_PERSONAL_ACCESS_TOKEN:-}" ]; then
    echo "Please set GITHUB_PERSONAL_ACCESS_TOKEN environment variable, this is a valid GitHub personal access token with access to the public github API \
        Or use the pat available in 1password vault at the same record."
    exit 1
fi

being_oracle (){
    export ANTI_WALLET_FILE=$ANTI_TEST_ORACLE_WALLET
    }
being_agent (){
    export ANTI_WALLET_FILE=$ANTI_TEST_AGENT_WALLET
    }
being_requester (){
    export ANTI_WALLET_FILE=$ANTI_TEST_REQUESTER_WALLET
    }

include_requests() {
    being_oracle
    validation=$(anti oracle requests validate)
    references=$(echo "$validation" | jq -r '.result | .[] | select(.validation == "validated") | .reference')
    if [ -z "$references" ]; then
        log "No references validated: $validation"
        exit 1
    fi
    # shellcheck disable=SC2046
    anti oracle token update $(echo "$references" | xargs -I {} echo -o {}) > /dev/null
}


being_agent
agent=$(anti wallet info | jq -r '.result.owner')
export ANTI_AGENT_PUBLIC_KEY_HASH=$agent

fund () {
    export ANTI_WALLET_FILE=$1
    address=$(anti wallet info | jq -r '.result.address')
    echo "Funding address: $address"
    curl -s -X 'POST' \
        "$ANTI_TEST_YACI_ADMIN/local-cluster/api/addresses/topup" \
        -H 'accept: */*' \
        -H 'Content-Type: application/json' \
        -d '{
        "address": "'"$address"'",
        "adaAmount": 10000
        }' | jq -r '.message'
}

# Fund the wallets
if [ -z "${ANTI_TEST_YACI_ADMIN:-}" ]; then
    echo "ANTI_TEST_YACI_ADMIN environment variable is not set, skipping funding wallets"
else
    fund "$ANTI_TEST_REQUESTER_WALLET"
    fund "$ANTI_TEST_ORACLE_WALLET"
    fund "$ANTI_TEST_AGENT_WALLET"
fi


log "Create an anti token"
being_oracle
result=$(anti oracle token boot)
tokenId=$(echo "$result" | jq -r '.result.value')
export ANTI_TOKEN_ID="$tokenId"
log "Anti token id $tokenId"

tokenEnd() {
    being_oracle
    log "Ending anti token $ANTI_TOKEN_ID..."
    anti oracle token end >/dev/null || echo "Failed to end the token"
}
trap 'tokenEnd' EXIT INT TERM

log "Register 'cfhal' as a GitHub user"
being_requester
anti requester register-user \
    --platform github \
    --username cfhal \
    --pubkeyhash  AAAAC3NzaC1lZDI1NTE5AAAAILjwzNvy87HbzYV2lsW3UjVoxtpq4Nrj84kjo3puarCH \
    > /dev/null

log "Include the user registration"
include_requests

log "Register cfhal as cardano-foundation/hal-fixture-sin repository antithesis test run requester"
being_requester
anti requester register-role \
    --platform github \
    --username cfhal \
    --repository cardano-foundation/hal-fixture-sin \
    > /dev/null

log "Include the role registration"
include_requests

log "Register a test run from cfhal to run an antithesis test on the cardano-foundation/hal-fixture-sin repository, \
     using the antithesis-test directory and commit 8e99893bf511dc75041b0347dc5af4bec54ce5d4, duration 1 hour, first try"
being_requester
anti requester create-test \
    --platform github \
    --username cfhal \
    --repository cardano-foundation/hal-fixture-sin \
    --directory antithesis-test \
    --commit 8e99893bf511dc75041b0347dc5af4bec54ce5d4 \
    --try 1 \
    --duration 1 \
    > /dev/null

log "Include the test run registration"
include_requests

log "Reject the test run with no reasons..."
being_agent
validation=$(anti agent query)
references=$(echo "$validation" | jq -r '.result | .pending | .[] | .id')
echo "References: $references"
anti agent reject-test -i "$references" > /dev/null

log "Include the test run rejection"
include_requests

log "Create a new test run request for the same repository, directory, and commit and duration, second try"
being_requester
anti requester create-test \
    --platform github \
    --username cfhal \
    --repository cardano-foundation/hal-fixture-sin \
    --directory antithesis-test \
    --commit 8e99893bf511dc75041b0347dc5af4bec54ce5d4 \
    --try 2 \
    --duration 1 \
    > /dev/null

log "Include the new test run request"
include_requests

log "Accept the new test run request because it's a scenario"
being_agent
validation=$(anti agent query)
references=$(echo "$validation" | jq -r '.result | .pending | .[] | .id')
anti agent accept-test -i "$references" > /dev/null

log "Include the test run acceptance"
include_requests

log "Finish the test run"
being_agent
validation=$(anti agent query)
references=$(echo "$validation" | jq -r '.result | .running | .[] | .id')
anti agent report-test -i "$references" \
    --duration 1 \
    --url "https://example.com/report" \
    > /dev/null

log "Include the test run report"
include_requests

log "Facts:"
anti facts | jq .result | jq .[]