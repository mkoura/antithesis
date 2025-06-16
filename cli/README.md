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

* how to implement 1?
  1. just use MPFS without authentication
  2. give back an unsigned tx for the user to sign
  3. run mpfs locally (as a requester)
