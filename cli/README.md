# Antithesis CLI

## Context

This is a command line interface (CLI) to track accesses to expensive resources. In particular we are focusing on the Cardano Foundation controlled Antithesis instance. The only command relative to the specific instance  is the `anti agent run-test` command, which is used to run tests on the Antithesis platform. The rest of the commands are independent of the resource. This CLI uses MPFS as a backend meaning all data is stored on the Cardano blockchain. ATM MPFS is only supporting preprod.
The CLI should be used to book tests on the Antithesis platform. ATM it supports only projects on GitHub.

## Prerequisites

Building the CLI requires access to specific libraries from the cardano stack. Using [Nix](https://nixos.org/download.html) is the easiest way to build the code, as it will bring in all the dependencies needed to build the CLI.

Running the CLI requires an MPFS backend. You can either run your own MPFS service or use a public one. A public one is hosted at `https://mpfs.plutimus.com`.

The CLI can run on Linux and MacOS.

## Installation via tarballs

You can download the latest tarball for your platform from the [releases page](https://github.com/cardano-foundation/antithesis/releases).

## Installation via Nix

### Nix cache

Be careful to be a trusted nix user so the caches indicated in the flake will kick in. Without them expect hours of compilation time.

### Install as nix shell

To get the last version of the code, you can use the following command:

```bash
nix shell github:cardano-foundation/antithesis?dir=cli#anti
```

### Building the tarballs

To build standalone packages with
- anti
- cardano-cli
- cardano-address
- bech32

On linux, you can build a nix derivation with

```bash
nix build github:cardano-foundation/antithesis?dir=cli#linux64.tarball
```

On macOS, you can build a nix derivation with

```bash
nix build .#macos64.tarball
```

## Running the CLI

### Completion

```bash
source <(anti --bash-completion-script `which anti`)
```

### Environemnt variables

####  Something to remove in the future

Rad issue 800db55

```bash
cat << EOF > config.json
{
    "minDuration": 1,
    "maxDuration": 12
}
EOF
export ANTI_CONFIG_FILE=config.json
```

#### MPFS host
If you do not want to host your own MPFS service, you can use a public one at `https://mpfs.plutimus.com`.

In any case set the `ANTI_MPFS_HOST` environment variable to point to the MPFS service you want to use.

```bash
export ANTI_MPFS_HOST=https://mpfs.plutimus.com
```

#### Your wallet

ATM the anti CLI works only by reading a wallet file containing a mnemonic phrase.

The anti command will read the wallet file from the `ANTI_WALLET_FILE` environment variable.

```bash
export ANTI_WALLET_FILE=wallet.json
```

You can create a wallet file with the `anti wallet create` command:

```bash
anti wallet create
```

It will fail to re-create the file if it already exists. You can review this wallet info anytime with

```bash
anti wallet info
```

> Fund your wallet with some tAda tokens on preprod, for example using the [Cardano Testnet Faucet](https://docs.cardano.org/cardano-testnets/tools/faucet/).
>
### Antithesis token

This is the unique token that identifies the Antithesis access interface. You need to refer to it setting the `ANTI_TOKEN_ID` environment variable.

```bash
export ANTI_TOKEN_ID=865ebcf5e1d6bafcc121030a6e167474a426271d965b78e36d90485adf540575
```


### Set the timeout for the `anti` command

When submitting txs to the chain, it's quite convenient to wait for the transaction to be included in the chain, so that you can immediately use the result of the transaction.

To do that, you can set the `ANTI_WAIT` environment variable to the number of seconds you want to wait for the transaction to be included in the chain.

```bash
export ANTI_WAIT=120
```

## Querying facts of the Antithesis token

You can always query the Antithesis token and its facts

```bash
anti facts | jq '.result'
```

## Design

[Design document](docs/antithesis-interface.md)

## Manuals

Depending on your role you can access the different manuals.

- [Test-runs Requester manual](docs/requester-role.md)
- [Oracle manual](docs/oracle-role.md)
- [Antithesis Agent manual](docs/antithesis-agent-role.md)
