#!/usr/bin/env bash

log() {
    echo "$(date '+%Y-%m-%d %H:%M:%S') - $1"
}

check() {
    if [ -z "$ANTI_WALLET_FILE" ]; then
        log "Error: ANTI_WALLET_FILE is not set."
        exit 1
    fi

    if [ ! -f "$ANTI_WALLET_FILE" ]; then
        log "Error: ANTI_WALLET_FILE does not exist at $ANTI_WALLET_FILE."
        exit 1
    fi
    log "Using ANTI_WALLET_FILE: $ANTI_WALLET_FILE"

    if [ -z "$ANTI_MPFS_HOST" ]; then
        log "Error: ANTI_MPFS_HOST is not set."
        exit 1
    fi
    log "Using ANTI_WALLET_FILE: $ANTI_WALLET_FILE"

    if ! command -v anti &>/dev/null; then
        log "Error: 'anti' command is not available. Please ensure it is installed and in your PATH."
        exit 1
    fi
}

printFacts() {
    log "Current facts:"
    anti facts | jq '.result.[]'
}

getOutputRef() {
    # shellcheck disable=SC2155
    local txHash=$(echo "$1" | jq -r '.result.txHash')
    echo "${txHash}-0"
}