#!/usr/bin/env bash

set -euo pipefail

# shellcheck disable=SC1091
source "$(dirname "$0")/lib.sh"

export ANTI_WAIT=240

unset ANTI_TOKEN_ID

result=$(anti oracle token boot)

tokenId=$(echo "$result" | jq -r '.result.value')

export ANTI_TOKEN_ID="$tokenId"

tokenEnd() {
    # log "Ending anti token $ANTI_TOKEN_ID..."
    anti oracle token end >/dev/null || echo "Failed to end the token"
}
trap 'tokenEnd' EXIT INT TERM

result=$(anti requester create-test \
    --platform test-hub \
    --repository test-org/test-repo \
    --directory test-dir \
    --commit test-commit \
    --username test-user \
    --try 1 \
    --duration 3)

outputRef=$(getOutputRef "$result")
anti oracle token update -o "$outputRef" >/dev/null

result=$(anti agent accept-test \
    --platform test-hub \
    --repository test-org/test-repo \
    --directory test-dir \
    --commit test-commit \
    --username test-user \
    --try 1)

outputRef=$(getOutputRef "$result")
anti oracle token update -o "$outputRef" >/dev/null

result=$(
    anti agent report-test \
        --platform test-hub \
        --repository test-org/test-repo \
        --directory test-dir \
        --commit test-commit \
        --username test-user \
        --try 1 \
        --duration 3 \
        --url "http://example.com/test-results"
)

outputRef=$(getOutputRef "$result")
anti oracle token update -o "$outputRef" >/dev/null

facts=$(anti facts | jq '.result')

expectedFacts=$(
    cat <<EOF
[
  {
    "key": {
      "type": "test-run",
      "commitId": "test-commit",
      "directory": "test-dir",
      "platform": "test-hub",
      "repository": {
        "organization": "test-org",
        "repo": "test-repo"
      },
      "requester": "test-user",
      "try": 1
    },
    "value": {
      "duration": 3,
      "from": {
        "from": {
          "duration": 3,
          "phase": "pending"
        },
        "phase": "accepted"
      },
      "phase": "finished",
      "url": "http://example.com/test-results"
    }
  }
]
EOF
)

if [[ "$(echo "$facts" | jq -S .)" != "$(echo "$expectedFacts" | jq -S .)" ]]; then
    log "Facts do not match expected output."
    log "Actual facts: $facts"
    log "Expected facts: $expectedFacts"
    exit 1
fi
