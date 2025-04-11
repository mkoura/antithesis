# compose

## Index

- [Information](#information)
- [Installation](#installation)
- [Plan](#plan)
- [Build](#build)
- [Run](#run)
  - [Local](#local)
  - [Antithesis](#antithesis)
- [Troubleshoot](#troubleshoot)
- [Appendix](#appendix)

## Information

This document describes the process of setting up a Cardano testnet from scratch. `cardano-node` can be compiled from source or downloaded as pre-compiled binary. The testnet will run inside of **Docker Containers** and will be controlled by **Docker Compose**.

## Installation

Docker and `make` are the only requirement to build and run your own testnets.

- Install `make` with your package manager

- Install Docker \
  https://docs.docker.com/engine/install

## Plan

- Create a copy of the `example_10.2.1` folder in a directory named after your own test network

  ```
  cd testnets/
  cp -r example_10.2.1 mytestnet
  cd mytestnet/
  ```

- Edit the `README.md` file to describe your test case and the version used

  ```
  editor README.md
  ```

- Modify the `testnet.yaml` file and insert your configuration details

  ```
  editor testnet.yaml
  ```

- Modify the `docker-compose.yaml` file to reflect your testnet defined above

  ```
  editor docker-compose.yaml
  ```

> [!CAUTION]
> Please ensure:
>   - The number of instances specified in the `docker-compose.yaml` matches the `poolCount` defined in `testnet.yaml`
>   - The appropriate `Dockerfile` is referenced correctly in `docker-compose.yaml` (either `Dockerfile.compiled` or `Dockerfile.source`)
>   - All build arguments (`args`) are accurately specified in `docker-compose.yaml`

## Build

- Move up two directories to the location where the Makefile is located

  ```
  cd ../../
  ```

- Lists all available commands and arguments

  ```
  make help
  ```

### Local

- Build the `cardano-node`, `config` and `sidecar` container images

  ```
  make build testnet=example_10.2.1
  ```

> [!IMPORTANT]
> When modifying either the `docker-compose.yaml` or `testnet.yaml` files, you must rebuild by running the build command.

### Antithesis

- Build the `cardano-node`, `config` and `sidecar` container images for the Antithesis container registry

  ```
  make build testnet=example_10.2.1 registry=us-central1-docker.pkg.dev/molten-verve-216720/cardano-repository/
  ```

> [!IMPORTANT]
> Always supply the registry argument when building for Antithesis.

## Run

### Local

#### Start Testnet

- Start a testnet

  ```
  make up testnet=example_10.2.1
  ```

#### Inspect Testnet

- Verify that all containers are running

  ```
  docker ps
  ```

- Check for consensus among all pools

  ```
  make validate testnet=example_10.2.1
  ```

- Query the tip of all pools

  ```
  make query testnet=example_10.2.1
  ```

#### Read Logs

- Read the logs of container `p1`

  ```
  docker logs --follow p1
  ```

- Find errors in container `p1`

  ```
  docker logs p1 | grep -i error
  ```

#### Execute Command

- List all available scripts inside of the `sidecar` container

  ```
  docker exec -ti s1 find /opt/composer/ -type f
  ```

- Execute a script inside of the `sidecar` container

  ```
  docker exec -ti s1 /opt/composer/convergence/eventually_converged.sh
  ```

- Dump a specific file of container `p1`

  ```
  docker exec -ti p1 cat /opt/cardano-node/pools/1/configs/shelley-genesis.json
  ```

#### Interactive Session

- Start an interactive terminal inside of container `p1` as service user

  ```
  docker exec -ti p1 /bin/bash
  ```

- Start an interactive terminal inside of container `p1` as root user

  ```
  docker exec --user root -ti p1 /bin/bash
  ```

#### Stop Testnet

- Stop the testnet

  ```
  make down testnet=example_10.2.1
  ```

### Antithesis

- Log in to the Antithesis container registry

  ```
  cat credentials.json | docker login -u _json_key https://us-central1-docker.pkg.dev --password-stdin
  ```

- Push the `cardano-node`, `config` and `sidecar` container images

  ```
  make push testnet=example_10.2.1 registry=us-central1-docker.pkg.dev/molten-verve-216720/cardano-repository/
  ```

> [!IMPORTANT]
> Always supply the registry argument when attempting to push container images to Antithesis.

- Trigger the default Antithesis job

  ```
  make anti testnet=example_10.2.1 password='password1234'
  ```

- Trigger a specific Antithesis job

  ```
  make anti testnet=example_10.2.1 password='password1234' url=https://cardano.antithesis.com/api/v1/launch/cardano
  ```

## Troubleshoot

- Query the start time in `byron-genesis.json` and `shelley-genesis.json`

  ```
  for i in {1..3} ; do docker exec -ti p${i} jq -er '.startTime' /opt/cardano-node/pools/${i}/configs/byron-genesis.json; done
  for i in {1..3} ; do docker exec -ti p${i} jq -er '.systemStart' /opt/cardano-node/pools/${i}/configs/shelley-genesis.json; done
  ```

- Query an option in `config.json`

  ```
  for i in {1..3} ; do docker exec -ti p${i} jq -er '.PeerSharing' /opt/cardano-node/pools/${i}/configs/config.json; done
  ```

> [!TIP]
> The commands above assume a testnet of 3 pools, increase the `{1..3}` if needed.

## Appendix

- [Antithesis Documentation](https://antithesis.com/docs/)
- [Docker Installation](https://docs.docker.com/engine/install)
- [testnet-generation-tool](https://github.com/cardano-foundation/testnet-generation-tool)
