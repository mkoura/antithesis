# Log Book

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
