# Antithesis CLI


## Design

* HAL Team holds the key
* trigger antithesis job with anti-cli for cardano-node from CI
* be able to trigger antithesis runs from "anywhere" (amaru, dingo)
* how user interacts with us?
  * chain is used to record trace of those interactions
* how do we provide results?

1. Who is creating the request to register a user?
  * anyone with maintainer rights can create requests
2. What does the request to run the test will contain?
  * a pointer to some antithesis configuration (docker-compose inside some repository at some commit)
3. Who is the owner of the request?
  * whoever is a maintainer of the repository (when?)

* MPFS = helper service for token owners (eg. useful to track requests and trigger runs)

We have 2 parts:
1. the "user", eg. team/person requesting test runs
2. ✅ AT managers -> use MPFS
   * sort out details about hosting
3. facts stored in MPF (key and values)

### Requester interface

how to implement request to register a user?
1. just use MPFS without authentication
2. give back an unsigned tx for the user to sign
3. run mpfs locally (as a requester)

req:
* requests should be signed by the actual requestor => request tx should be signed by this "person"
* we don't really need the indexing for the user role, but it's easier/faster to do

constraint:
* MPFS is in TS and works as HTTP service
* `anti cli` is in Haskell

Decision for v0.1:
* we deploy a centralised MPFS for anyone to use
* anti cli interacts with this centralised service
* signing still happens locally (eg. MPFS is non-custodial)

for registration/requests, user can:
* get a balanced tx  by providing an address
* get an unbalanced tx and to the balancing/signing
* get a raw datum to construct the request themselves

for retract:
* we should also serve the script

BTW, MPFS is a very good tool for tracking scripts' blueprints

### Next steps

#### MPFS

* Expand request endpoints to allow passing address argument for balancing
  * Return CBOR when passing an address

#### Anti Cli

* define the protocol's key/value data structures -> JSON schema

## Example workflow for registering a user with a public key

`alice` sets up an `mpfs` service listening on port 3000

`oracle` sets up an(other) `mpfs` service listening on port 3000

`oracle` create a token to track antithesis interactions

```bash
> tokenId=$(anti oracle token create | jq -r .tokenId)
> echo $tokenId
7775e63684562092db55e0cdecbfefbb9d506164a2c4fde66c13e3c709604276
```
`alice` store the token id

```bash
tokenId=7775e63684562092db55e0cdecbfefbb9d506164a2c4fde66c13e3c709604276
```

`alice` request to register herself (or any GH user) as an identified user with some public key in the `token`

``` bash
> anti user request register-public-key --platform github --username alice --pubkeyhash AAAAC3NzaC1lZDI1NTE5AAAAIO773JHqlyLm5XzOjSe+Q5yFJyLFuMLL6+n63t4t7HR8 -t   $tokenId | jq -r
f36c501b5081db1a40df949749264fb76c0a6c8460976825faaa0c651f1f74e6-0
```

`oracle` collect current requests

```bash
> anti oracle token get -t $tokenId | jq '.requests | .[] | {key: .change.key, value: .change.value, ref: .outputRef}'
{
  "key": "register-public-key/github/alice/AAAAC3NzaC1lZDI1NTE5AAAAIO773JHqlyLm5XzOjSe+Q5yFJyLFuMLL6+n63t4t7HR8",
  "value": "",
  "ref": "f36c501b5081db1a40df949749264fb76c0a6c8460976825faaa0c651f1f74e6-0"
}
```

`oracle` checks that the request is valid (TO_DO in 802bf7)

```bash
> anti oracle github --key register-public-key/github/alice/AAAAC3NzaC1lZDI1NTE5AAAAIO773JHqlyLm5XzOjSe+Q5yFJyLFuMLL6+n63t4t7HR8 --value ""
true
```

`oracle` merges the request into the `token`

```bash
> anti oracle token update -t $tokenId -o f36c501b5081db1a40df949749264fb76c0a6c8460976825faaa0c651f1f74e6-0
```

now `alice` can inspect the token facts

```bash
> anti user request facts -t $tokenId | jq
{
  "register-public-key/github/alice/AAAAC3NzaC1lZDI1NTE5AAAAIO773JHqlyLm5XzOjSe+Q5yFJyLFuMLL6+n63t4t7HR8": ""
}
```

  <!-- * `oracle` merges the fact (via a cardano transaction) in the `token` when
    * `github` attests `alice` credentials as in the request

* `alice` waits for her identity to be registered by polling the chain to inspect the `token`
    * command: `anti query --username alice`
* `alice` registers her role as maintainer for repo `IntersectMBO/cardano-node-test`
  * command: `anti register-role --platform github --repository IntersectMBO/cardano-node-test --username alice --role maintainer`
  * fact:
    * key: `platform/repo/"maintainer"`
    * value: `{username: alice}`
  * `oracle` merges the fact into `token` when
    *  the `token` attests `alice` credentials user
    *  `github` attests `alice` role in the repository
* `alice` waits for her role to be registered  by polling the chain to inspect the `token`
    * command:  `anti query --username alice`
* now `alice` and `antithesis` will interact using the token state as interface
  * `alice` requests to run a new test
    * command: `anti request-test-run --platform github --repository IntersectMBO/cardano-node-test --commit 234456 --username alice`
    * fact:
      * key: `platform/repo/commit/"test-run"/run-index`
      * value: `{state: request, conditions: {timeout: 1d}}`
    * the test is described by the content of the repository (possibly with some subdirectory) at the given commit
      * need to document this and fill in details...
    * `oracle` merges `alice` request to create a test-run when
        * `alice` is a maintainer of the repo is a fact in the `token`
      * the `platform/repo/commit` prefix is not present in the `token` and the `run-index` is 0, or `run-index` - 1 is present for the `platform/repo/commit` prefix
  * `antithesis`, polls the `token` for requests to run and process `alice` request to test-run
      * if it doesn't accept the request conditions, `antithesis` requests to update the `token` with a rejection
        * command: `anti reject-test-run --platform github --repository IntersectMBO/cardano-node-test --commit 234456`
        * fact:
          * key: `platform/repo/commit/"test-run"/run-index`
          * value: `{state: rejected}`
        *  `oracle` merges `antithesis` request to update the token at index when
          * the request is signed by the hard-coded antithesis identity
          * the value for the key in the request is in `request` state
      * if it does accept it creates a request for the oracle to update the `token` with an acceptance
        * command: `anti accept-test-run --platform github --repository IntersectMBO/cardano-node-test --commit 234456`
        * fact:
          * key: `platform/repo/commit/"test-run"/run-index`
          * value: `{state: accepted}`
        * `oracle` merges `antithesis` request to update the token at index when
          * the request is signed by the hard-coded antithesis identity
          * the value for the key in the request is in `request` state
  * `antithesis`, which is also polling the antithesis platform, request to update the `token` with the test-run results
    * command: `anti response-test-run --platform github --repository IntersectMBO/cardano-node-test --commit 234456 --ipfs cbe89a098cfbae890db8fa0bcf8e9`
    * fact:
      * key `platform/repo/commit/"test-run"/run-index`
      * value `{state: finished, results: cbe89a098cfbae890db8fa0bcf8e9}`
    * `oracle` merges `antithesis` request to update the token at index when
       * the request is signed by the hard-coded antithesis identity
       * the value for the key in the request is in `progress` state -->
