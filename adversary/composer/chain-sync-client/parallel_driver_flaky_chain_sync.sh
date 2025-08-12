#!/usr/bin/env bash

set -o pipefail

SHELL="/bin/bash"
PATH="/sbin:/bin:/usr/sbin:/usr/bin:/usr/local/sbin:/usr/local/bin"

# Environment variables
POOLS="${POOLS:-}"
mapfile -t NODES < <(seq -f "p%g" 1 "$POOLS")
set -f
NODES+=( $EXTRA_NODES )
set +f

PORT="${PORT:-3001}"

echo "Checking flaky chain sync among the following nodes"
printf '%s\n' "${NODES[@]}"

# TODO: select randomly
tested_pool = ${NODES[0]}

adversary $tested_pool