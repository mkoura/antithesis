# syntax=docker/dockerfile:1.4
ARG CARDANO_CLI_VERSION="${CARDANO_CLI_VERSION:-10.8.0.0}"
ARG UV_VERSION="${UV_VERSION:-0.6.11}"

# Blink Labs images are built from source on Debian Bookworm
FROM ghcr.io/blinklabs-io/cardano-node:latest AS cardano-node

FROM ghcr.io/astral-sh/uv:${UV_VERSION} AS uv

FROM debian:stable-slim@sha256:377ddc2a20fe8632a49b69dcfff10fccbd5b4f0b8c2d593420a6a5e03070dfa1

USER root
# Install packages required in build stage
RUN apt update && \
    apt install -y --no-install-recommends \
        ca-certificates \
        curl \
        git \
        tar \
        tree \
        jq
# Copy uv from image
COPY --from=uv --chown=root:root /uv /usr/local/bin/uv

# Create cardano-node source directory
RUN install --directory --owner=root --group=root --mode=0755 /usr/local/src/cardano-node

# Clone testnet-generation-tool.git repository
WORKDIR /usr/local/src
RUN git clone --branch main https://github.com/cardano-foundation/testnet-generation-tool.git

# Download testnet-generation-tool dependencies
WORKDIR /usr/local/src/testnet-generation-tool
# Copy binary from image
COPY --from=cardano-node --chown=root:root /usr/local/bin/ ./cardano-node/bin/
# Copy libsecp256k1 (also debian:bookwork-slim AKA stable-slim)
COPY --from=cardano-node --chown=root:root /usr/lib/ /usr/lib/
RUN uv sync

COPY configurator.sh /
RUN chmod 0755 /configurator.sh

ENTRYPOINT ["/configurator.sh"]
