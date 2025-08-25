#!/usr/bin/env bash

set -euo pipefail

# shellcheck disable=SC1091
source "$(dirname "$0")/lib.sh"

export ANTI_WAIT=240
unset ANTI_TOKEN_ID

check

fund_wallets

log "Create an anti token"
being_oracle
result=$(anti oracle token boot)
tokenId=$(echo "$result" | jq -r '.value')
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
result=$(anti requester register-user \
    --platform github \
    --username cfhal \
    --pubkeyhash  AAAAC3NzaC1lZDI1NTE5AAAAILjwzNvy87HbzYV2lsW3UjVoxtpq4Nrj84kjo3puarCH \
    )

log "Retract the user registration"
being_requester
outputRef=$(getOutputRef "$result")
anti retract -o "$outputRef"

result=$(anti token | jq '.requests')
if [[ "$result" == "[]" ]]; then
    log "Test passed: User registration successfully retracted"
else
    log "Test failed: Retraction not reflected in requests"
    exit 1
fi
