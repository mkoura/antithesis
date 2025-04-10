# Log Book

## 2025-04-09

### Meeting summary

* We had our weekly touchpoint with AT team and CF Task Force
* Over the past week we refined our understand of the platform and the CF team is working on a branch with several different setups, including one "large" cluster test where P2P networking can [lead to a crash](https://github.com/IntersectMBO/ouroboros-network/issues/5058) on some particular circumstances
* We discussed our difficulties chasing that particular network bug which requires more runtime than is customary with AT runs
  * We can reproduce the bug using a local docker compose setup so we know it's possible to do it
  * We could not find a way to "miniaturise" the bug, e.g reduce network size, parameters, slot duration, $k$, etc.
* A discussion about the kind of faults we are looking at ensued:
  * network related faults, like the one above, which should be related to handling of adversarial conditions in the system
  * consensus faults which are related to the diffusion logic and chain selection, also depend on resources (eg. selection and diffusiong needs to happen quickly) but mostly related to correct implementation of Ouroboros Praos protocol
  * mempool faults, which are related to diffusion of txs, and can have adversarial effect on the system because of excessive use of resources, race conditions, competing with other parts, etc.
  * ledger faults which relate to the block/transaction evaluation
* AT work should focus (at least for now?) on the first three as ledger is a pure function, although it could be the case an error in the ledger ripples to other layers (eg. diverging computations, unexpected errors, etc.)
  * we note that all layers have an extensive set of property tests
  * it's still unclear how to be best use the tools. AT engine uses different computing characteristics for different workloads/SUTs
  * It's possible to calibrate those performance characteristics in order to better replicate environment but this is not open to customers
* One problem we was the logs output limit. Its purpose is to help reproducing things faster as obviously more output leads to more resources for each run
* While there are certainly bugs that can be triggered through fuzzing I/Os, we need to have a way to run "adversaries" within the system, to inject blocks/transactions to load the system and possibly trigger issues in consensus, mempool, or ledger
  * We don't currently deploy anything like that in our stack, but we should build something
* We discuss how AT does fault injection
  * it's completely rerandomized on purpose, in order to remove human biases as much as possible
  * with more information about baseline guarantees expected from the system, some issues found could be filter out, eg. triggering errors that are beyond the operational limits of the system
  * there's no API to control the various parameters for fault injection

TODOs:

1. extend test execution time to reproduce network bug, and explore how AT analysis can help to understand the bug
1. try to miniaturise the setup to reproduce the bug
2. try to reproduce a consensus bug that requires injection of data
   a. implies we need to build some tool to inject blocks/data in the network
3. defining & refining general security properties
4. integrate [cardano-tracer](https://developers.cardano.org/docs/get-started/cardano-node/new-tracing-system/cardano-tracer/) into compose stack to be able to collect logs
5. express assertion on logs collected with tracer

## 2025-04-02

### Official kick-off meeting

* Antithesis presentation plan:
  * containerize SUT
  * designing tests
  * PBT and what that means, what kind of invariants we can write

* Platform needs:
  * docker images x86_64 archi
  * docker-compose.yaml
  * continuous build pipeline -> push images to AT registry
  * continuous testing
  * implement SSO w/ OIDC (new features, multiverse debugging, private reports)
    * Google workspace w/ SAML?
    * support GitHub auth in the future

* maximize benefits from AT
  * miniaturize setup -> small number of nodes
  * code coverage instrumentation (easy to do in Rust, there's also C instrumentation)
  * use [test composer](https://antithesis.com/docs/test_templates/) to write workload

* antithesis output:
  * webhook endpoint -> trigger the test
  * email -> triage report
  * email -> bug report, statistical tool showing replays/simulations before/after bug point => probability of when in time bugs occur
  * multiverse debugger -> replay tool, allow to run batch command, need to think about debugging workflow (eg. package tools inside images to ease debugging process)
  * reports is available after tests run now, will be available live later on

* no coverage -> default to fuzzing and fault injection, can guide the fuzzer leverage SDK (eg. unreachable)
  * sidecar, more easily instrumente than the actual node

Next steps:

* Credentials to access the platform
* Sharing some documentation and guidelines
* Let's get our first test running :rocket:

## 2025-04-01

### Antithesis Project Pre-kick-off meeting

* Overview of the scope of the project, the deadlines, the goal(s), who's involved, and possible outcomes (see [Antithesis' README](./antithesis/README.md)
* discussed how to get started, what's there, what needs to happen
* Got a demo of some tests the team has already worked on that could be good candidate for onboarding and PoC
* Suggestion: we could use cardano-wallet as a driver to generate transactions?
  * would need to have a way to control several wallets
  * Might be overkill at least at start
