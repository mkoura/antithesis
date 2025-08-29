FROM docker.io/debian@sha256:8810492a2dd16b7f59239c1e0cc1e56c1a1a5957d11f639776bd6798e795608b AS build

ARG BLST_VERSION="${BLST_VERSION:-0.3.11}"
ARG BOOTSTRAP_HASKELL_CABAL_VERSION="${BOOTSTRAP_HASKELL_CABAL_VERSION:-3.12.1.0}"
ARG BOOTSTRAP_HASKELL_GHC_VERSION="${BOOTSTRAP_HASKELL_GHC_VERSION:-9.6.7}"
ARG CARDANO_CLI_VERSION="${CARDANO_CLI_VERSION:-10.5.0.0}"
ARG CARDANO_NODE_REF="${CARDANO_NODE_REF:-10.2.1}"
ARG CARDANO_NODE_REPO="${CARDANO_NODE_REPO:-https://github.com/IntersectMBO/cardano-node.git}"
ARG JOBS="${JOBS:-8}"
ARG LIBSODIUM_VERSION="${LIBSODIUM_VERSION:-dbb48cc}"
ARG SECP256K1_VERSION="${SECP256K1_VERSION:-ac83be33}"
ARG UV_VERSION="${UV_VERSION:-0.6.11}"

# Set time zone
ENV TZ="UTC"
RUN ln -snf /usr/share/zoneinfo/${TZ} /etc/localtime && \
    echo ${TZ} > /etc/timezone

# Install packages required in build stage
RUN apt update && \
    apt install -y --no-install-recommends \
        autoconf \
        automake \
        build-essential \
        ca-certificates \
        curl \
        g++ \
        git \
        gnupg \
        libffi-dev \
        libgmp-dev \
        liblmdb-dev \
        libncurses-dev \
        libssl-dev \
        libsystemd-dev \
        libtinfo-dev \
        libtool \
        pkg-config \
        tar \
        zlib1g-dev

# Install GHCup
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# Activate GHCup
RUN ln -s /root/.ghcup/bin/cabal-${BOOTSTRAP_HASKELL_CABAL_VERSION} /usr/local/bin/cabal && \
    ln -s /root/.ghcup/ghc/${BOOTSTRAP_HASKELL_GHC_VERSION}/bin/ghc-${BOOTSTRAP_HASKELL_GHC_VERSION} /usr/local/bin/ghc

# Clone libsodium.git repository
WORKDIR /usr/local/src
RUN git clone --branch master https://github.com/IntersectMBO/libsodium.git

# Build libsodium
WORKDIR /usr/local/src/libsodium
RUN git checkout ${LIBSODIUM_VERSION} && \
    ./autogen.sh && \
    ./configure && \
    make && \
    make check && \
    make install -j${JOBS}

# Clone secp256k1.git repository
WORKDIR /usr/local/src
RUN git clone --branch master https://github.com/bitcoin-core/secp256k1.git

# Build secp256k1
WORKDIR /usr/local/src/secp256k1
RUN git checkout ${SECP256K1_VERSION} && \
    ./autogen.sh && \
    ./configure --prefix=/usr --enable-module-schnorrsig --enable-experimental && \
    make && \
    make check && \
    make install -j${JOBS}

# Clone blst.git repository
WORKDIR /usr/local/src
RUN git clone --branch master https://github.com/supranational/blst.git

# Build blst
WORKDIR /usr/local/src/blst
RUN git checkout v${BLST_VERSION} && \
    ./build.sh
RUN echo "prefix=/usr/local\n\
exec_prefix=/usr/local\n\
libdir=/usr/local/lib\n\
includedir=/usr/local/include\n\
\n\
Name: libblst\n\
Description: Multilingual BLS12-381 signature library\n\
URL: https://github.com/supranational/blst\n\
Version: ${BLST_VERSION}\n\
Cflags: -I/usr/local/include\n\
Libs: -L/usr/local/lib -lblst" >/usr/local/src/blst/libblst.pc

RUN mv /usr/local/src/blst/libblst.pc /usr/local/lib/pkgconfig/ && \
    mv /usr/local/src/blst/bindings/blst.h /usr/local/include/ && \
    mv /usr/local/src/blst/bindings/blst.hpp /usr/local/include/ && \
    mv /usr/local/src/blst/bindings/blst_aux.h /usr/local/include/ && \
    mv /usr/local/src/blst/libblst.a /usr/local/lib

# Update path environment variables
ENV LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH" \
    PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH"

# Clone cardano-node.git repository
WORKDIR /usr/local/src
COPY cabal.project .
COPY adversary.cabal .
RUN cabal update && cabal build --only-dependencies
COPY . .
RUN cabal build all
RUN ln -s "$(cabal list-bin adversary)" /usr/local/bin/adversary

#---------------------------------------------------------------------

FROM docker.io/debian:stable-slim AS main

# Set time zone
ENV TZ="UTC"
RUN ln -snf /usr/share/zoneinfo/${TZ} /etc/localtime && \
    echo ${TZ} > /etc/timezone

# lmdb library
RUN apt update && apt install -y --no-install-recommends liblmdb-dev

# Copy libsodium library
COPY --from=build --chown=root:root /usr/local/lib/libsodium.so /usr/local/lib
RUN ln -snf /usr/local/lib/libsodium.so /usr/local/lib/libsodium.so.23 && \
    ln -snf /usr/local/lib/libsodium.so /usr/local/lib/libsodium.so.23.3.0
RUN ldconfig

# Copy secp256k1 library
COPY --from=build --chown=root:root /usr/local/src/secp256k1/.libs/libsecp256k1.so /usr/local/lib
RUN ln -snf /usr/local/lib/libsecp256k1.so /usr/local/lib/libsecp256k1.so.1 && \
    ln -snf /usr/local/lib/libsecp256k1.so /usr/local/lib/libsecp256k1.so.1.0.1
RUN ldconfig

# Copy binaries from build stage
COPY --from=build --chown=root:root /usr/local/bin/adversary /usr/local/bin/adversary

# Create cardano group and user
RUN groupadd --gid 10000 cardano && \
    useradd --comment 'cardano' --create-home --gid 10000 --password '!' --shell '/bin/bash' --uid 10000 cardano

# Copy Antithesis scripts
COPY composer /opt/antithesis/test/v1/
RUN chmod 0755 /opt/antithesis/test/v1/*/*

COPY sleep.sh .
RUN chmod 0755 ./sleep.sh

ENTRYPOINT ["./sleep.sh"]
