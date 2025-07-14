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

resultReg1=$(anti requester register-user \
    --platform github \
    --username paolino \
    --pubkeyhash AAAAC3NzaC1lZDI1NTE5AAAAIO773JHqlyLm5XzOjSe+Q5yFJyLFuMLL6+n63t4t7HR8)

outputRegRef1=$(getOutputRef "$resultReg1")
log "Created registration request with valid public key with output reference: $outputRegRef1"

if [ -z "$GITHUB_PERSONAL_ACCESS_TOKEN" ]; then
    log "Error: GITHUB_PERSONAL_ACCESS_TOKEN is not set. Please refer to \"https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens\#creating-a-fine-grained-personal-access-token\""
    exit 1
fi

resultVal1=$(anti oracle requests validate | jq -r '.result')

expectedVal1=$(
    cat <<EOF
[
  {
    "reference": "$outputRegRef1",
    "validation": "validated"
  }
]
EOF
)

emitValidationMismatch() {
    log "Validation result does not match expected value:"
    log "    Actual validation $1 result: $2"
    log "    Expected validation $1 result: $3"
    exit 1
}

if [[ "$(echo "$resultVal1" | jq -S .)" != "$(echo "$expectedVal1" | jq -S .)" ]]; then
    emitValidationMismatch 1 "$resultVal1" "$expectedVal1"
fi

resultReg2=$(anti requester register-user \
    --platform github \
    --username paolino \
    --pubkeyhash AAAAC3NzaC1lZDI1NTE5AAAAIO773JHqlyLm5XzOjSe+Q5yFJyLFuMLL6+n63t4t7HR9)

outputRegRef2=$(getOutputRef "$resultReg2")
log "Created registration request with invalid public key with output reference: $outputRegRef2"

resultVal2=$(anti oracle requests validate | jq -r '.result')

expectedVal2=$(
    cat <<EOF
[
  {
    "reference": "$outputRegRef1",
    "validation": "validated"
  },
  {
    "reference": "$outputRegRef2",
    "validation": "not validated: The user does not have the specified Ed25519 public key exposed in Github."
  }
]
EOF
)

if [[ "$(echo "$resultVal2" | jq -S .)" != "$(echo "$expectedVal2" | jq -S .)" ]]; then
    emitValidationMismatch 2 "$resultVal2" "$expectedVal2"
fi


#log "Including the registration request in the token ..."
#anti oracle token update -o "$outputRegRef1" >/dev/null

#printFacts
