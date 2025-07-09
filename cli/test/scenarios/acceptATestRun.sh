#!/usr/bin/env bash

set -euo pipefail

# shellcheck disable=SC1091
source "$(dirname "$0")/lib.sh"

export ANTI_WAIT=240

unset ANTI_TOKEN_ID

log "Using ANTI_MPFS_HOST: $ANTI_MPFS_HOST"

log "Creating an anti token..."
result=$(anti oracle token boot)

tokenId=$(echo "$result" | jq -r '.result.value')
log "Anti token ID: $tokenId"

export ANTI_TOKEN_ID="$tokenId"

tokenEnd() {
    log "Ending anti token $ANTI_TOKEN_ID..."
    anti oracle token end >/dev/null || echo "Failed to end the token"
}
trap 'tokenEnd' EXIT INT TERM

log "Creating a test-run request..."

result=$(anti requester create-test \
    --platform test-hub \
    --repository test-org/test-repo \
    --directory test-dir \
    --commit test-commit \
    --username test-user \
    --try 1 \
    --duration 3)

outputRef=$(getOutputRef "$result")
log "Created test-run request with output reference: $outputRef"

log "Including the test-run request in the token ..."
anti oracle token update -o "$outputRef" >/dev/null

printFacts

log "Accepting the test-run request..."
result=$(anti agent accept-test \
    --platform test-hub \
    --repository test-org/test-repo \
    --directory test-dir \
    --commit test-commit \
    --username test-user \
    --try 1)

outputRef=$(getOutputRef "$result")
log "Created a test-run rejection with output reference: $outputRef"

log "Including the test-run rejection in the token ..."
anti oracle token update -o "$outputRef" >/dev/null

printFacts
