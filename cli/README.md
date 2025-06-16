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
