# Antithesis Agent Role

## White-list repositories

In the interest of avoiding spam the agenthas to white-list repositories before the oracle will accept test-runs for them.

Two commands are available

### White-list a repository

This will only work if the repository is not already white-listed and the repository is in GitHub.
```bash
anti agent white-list <platform> <repository>
```

ATM only GitHub is supported as a platform.
The format of the repository is `<owner>/<repository>`, e.g. `cardano-foundation/antithesis`.

### Black-list a repository

This will only work if the repository is white-listed.
```bash
anti agent black-list <platform> <repository>
```

### 

TBD