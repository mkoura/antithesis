# Example workflow for registering a user with a public key

`alice` sets up an `mpfs` service listening on port 3000

`oracle` sets up an(other) `mpfs` service listening on port 3000

`oracle` create a token to track antithesis interactions

```bash
> tokenId=$(anti oracle create-token | jq -r .tokenId)
> echo $tokenId
7775e63684562092db55e0cdecbfefbb9d506164a2c4fde66c13e3c709604276
```
`alice` store the token id

```bash
tokenId=7775e63684562092db55e0cdecbfefbb9d506164a2c4fde66c13e3c709604276
```

`alice` request to register herself (or any GH user) as an identified user with some public key in the `token`

``` bash
> anti user register-public-key --platform github --username alice --pubkeyhash AAAAC3NzaC1lZDI1NTE5AAAAIO773JHqlyLm5XzOjSe+Q5yFJyLFuMLL6+n63t4t7HR8 -t   $tokenId | jq -r
f36c501b5081db1a40df949749264fb76c0a6c8460976825faaa0c651f1f74e6-0
```

`oracle` collect current requests

```bash
> anti oracle get-token -t $tokenId | jq '.requests | .[] | {key: .change.key, value: .change.value, ref: .outputRef}'
{
  "key": "register-public-key/github/alice/AAAAC3NzaC1lZDI1NTE5AAAAIO773JHqlyLm5XzOjSe+Q5yFJyLFuMLL6+n63t4t7HR8",
  "value": "",
  "ref": "f36c501b5081db1a40df949749264fb76c0a6c8460976825faaa0c651f1f74e6-0"
}
```

`oracle` checks that the request is valid

```bash
> anti oracle github --key register-public-key/github/alice/AAAAC3NzaC1lZDI1NTE5AAAAIO773JHqlyLm5XzOjSe+Q5yFJyLFuMLL6+n63t4t7HR8 --value ""
true
```

`oracle` merges the request into the `token`

```bash
> anti oracle update-token -t $tokenId -o f36c501b5081db1a40df949749264fb76c0a6c8460976825faaa0c651f1f74e6-0
```

now `alice` can inspect the token facts

```bash
> anti user get-facts -t $tokenId | jq
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
