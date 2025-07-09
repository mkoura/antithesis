#!/usr/bin/env bash

set -euo pipefail

# shellcheck disable=SC1091
source "$(dirname "$0")/lib.sh"

export ANTI_WAIT=180

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

log "Creating a registration user request..."
