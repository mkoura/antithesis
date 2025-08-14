#!/usr/bin/env bash

log() {
    echo "$(date '+%Y-%m-%d %H:%M:%S') - $1"
}

check() {
    if ! command -v anti &> /dev/null; then
        echo "anti command not found, please make it available in your PATH"
        exit 1
    fi
    if [ -z "$ANTI_WALLET_FILE" ]; then
        log "Error: ANTI_WALLET_FILE is not set."
        exit 1
    fi

    if [ ! -f "$ANTI_WALLET_FILE" ]; then
        log "Error: ANTI_WALLET_FILE does not exist at $ANTI_WALLET_FILE."
        exit 1
    fi

    if ! command -v anti &>/dev/null; then
        log "Error: 'anti' command is not available. Please ensure it is installed and in your PATH."
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
}

printFacts() {
    log "Current facts:"
    anti facts | jq '.result | .[]'
}

getOutputRef() {
    # shellcheck disable=SC2155
    local txHash=$(echo "$1" | jq -r '.result.txHash')
    echo "${txHash}-0"
}

fund () {
    export ANTI_WALLET_FILE=$1
    address=$(anti wallet info | jq -r '.result.address')
    log "Funding address: $address"
    curl -s -X 'POST' \
        "$ANTI_TEST_YACI_ADMIN/local-cluster/api/addresses/topup" \
        -H 'accept: */*' \
        -H 'Content-Type: application/json' \
        -d '{
        "address": "'"$address"'",
        "adaAmount": 10000
        }' > /dev/null
}

fund_wallets(){
    if [ -z "${ANTI_TEST_YACI_ADMIN:-}" ]; then
        echo "ANTI_TEST_YACI_ADMIN environment variable is not set, skipping funding wallets"
    else
        fund "$ANTI_TEST_REQUESTER_WALLET"
        fund "$ANTI_TEST_ORACLE_WALLET"
        fund "$ANTI_TEST_AGENT_WALLET"
    fi
}

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

export_agent_public_key_hash() {
    being_agent
    agent=$(anti wallet info | jq -r '.result.owner')
    export ANTI_AGENT_PUBLIC_KEY_HASH=$agent
}
