#!/usr/bin/env bash

set -euo pipefail

log() {
    echo "$(date '+%Y-%m-%d %H:%M:%S') - $1"
}

if [ -z "$ANTI_WALLET_FILE" ]; then
    log "Error: ANTI_WALLET_FILE is not set."
    exit 1
fi

if [ ! -f "$ANTI_WALLET_FILE" ]; then
    log "Error: ANTI_WALLET_FILE does not exist at $ANTI_WALLET_FILE."
    exit 1
fi

if [ -z "$ANTI_MPFS_HOST" ]; then
    log "Error: ANTI_MPFS_HOST is not set."
    exit 1
fi

if ! command -v anti &> /dev/null; then
    log "Error: 'anti' command is not available. Please ensure it is installed and in your PATH."
    exit 1
fi

export ANTI_WAIT=240

unset ANTI_TOKEN_ID

log "Using ANTI_WALLET_FILE: $ANTI_WALLET_FILE"
log "Using ANTI_MPFS_HOST: $ANTI_MPFS_HOST"

printFacts() {
    log "Current facts:"
    anti facts | jq
}

getOutputRef() {
    # shellcheck disable=SC2155
    local txHash=$(echo "$1" | jq -r '.result.txHash')
    echo "${txHash}-0"
}

log "Creating anti oracle token..."
result=$(anti oracle token boot)

tokenId=$(echo "$result" | jq -r '.result.value')
log "Created anti oracle token with ID: $tokenId"

export ANTI_TOKEN_ID="$tokenId"

tokenEnd() {
    log "Ending anti oracle token..."
    anti oracle token end > /dev/null || echo "Failed to end the token"
}
trap 'tokenEnd' EXIT INT TERM

log "User creating a test-run request..."

result=$(anti requester create-test \
    --platform test-hub \
    --repository test-org/test-repo \
    --directory test-dir \
    --commit test-commit \
    --username test-user \
    --try 1 \
    --duration 3)

outputRef=$(getOutputRef "$result")
log "Test run request created with output reference: $outputRef"

log "Oracle accepting the test run request in the facts"
anti oracle token update -o "$outputRef" > /dev/null

printFacts

log "Agent rejecting the test run request..."
result=$(anti agent reject-test \
    --platform test-hub \
    --repository test-org/test-repo \
    --directory test-dir \
    --commit test-commit \
    --username test-user \
    --try 1 \
    --reason "I am not ready yet")

outputRef=$(getOutputRef "$result")
log "Test run request rejected with output reference: $outputRef"

log "Oracle updating the token with the fact state update..."
anti oracle token update -o "$outputRef" > /dev/null

printFacts
