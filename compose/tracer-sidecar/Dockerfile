FROM haskell:9.6 AS build

WORKDIR /opt/tracer-sidecar

# Add just the .cabal and .project files to capture dependencies
COPY ./tracer-sidecar.cabal .
COPY ./cabal.project .

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN cabal update && cabal build --only-dependencies -j4

# Add and Install Application Code
COPY . .
RUN cabal install

FROM docker.io/debian:stable-slim AS main

COPY --from=build /root/.local/bin/tracer-sidecar /usr/local/tracer-sidecar

CMD ["/usr/local/tracer-sidecar", "/opt/cardano-tracer"]
