# Requester role

This is the role of the user who wants to run tests using the Antithesis platform.

Before you can request a test run, you need to register yourself as github user with an ed25519 public key.

## Registrations

A couple of requests have to be made once before the regular test-run requests can succeed. These requests are used to register the user and the project on the Antithesis platform.

ATM we only support Github as a platform, but we plan to support other platforms in the future.

Registration and unregistrations can be requested by anyone. The oracle role will simply validate the facts against the Github platform and update the Antithesis token accordingly. You cannot register a user or project that is already registered.

Be careful that there is no imperativity here, so i.e. you cannot unregister a user public key if it's present in Github.

### Registering a user public key

To register yourself as a user, you can use the `antij requester register-user` command.

```bash
antij requester register-user --platform github --username alice --pubkeyhash AAAAC3NzaC1lZDI1NTE5AAAAIO773JHqlyLm5XzOjSe+Q5yFJyLFuMLL6+n63t4t7HR8
```

As with all other requests, once submitted regularly you have to wait for the oracle to merge your request into the Antithesis token.

You can use the `antij token` command to inspect your pending requests in the Antithesis token.

You can use the `antij facts` command to query the Antithesis token and see if your user is part of the facts.

```bash
antij token | jq '.result.requests' "
```

Until your requests is there, you cannot proceed with the next steps.

As with all requests to an mpfs you can retract your request using the `antij retract` command, anytime before the oracle merges it into the Antithesis token.

Get the `outputRefId` of your request from pending requests command output and use it to retract your request

```bash
antij retract -o 9ec36572e01bca9f7d32d791a5a6c529ef91c10d536f662735af26311b2c8766-0
```
ATM the oracle is not able to justify a request rejection. But antij cli will apply the oracle validation before submitting it, so rejections will be caught before submitting the request.

### Unregistering a user public key

To unregister a user, you can use the `antij requester unregister-user` command.

```bash
antij requester unregister-user --platform github --username alice --pubkeyhash AAAAC3NzaC1lZDI1NTE5AAAAIO773JHqlyLm5XzOjSe+Q5yFJyLFuMLL6+n63t4t7HR8
```

### Registering a role

This is necessary to register a user as a github repository antitheisis test requester.

Before you do this make sure your repository CODEOWNERS file contains a line like this:

```
antithesis: @your-github-username
```

You  can have as many user as you want but registering them as test-run requesters has to be done one by one.

To register a role, you can use the `antij requester register-role` command.

```bash
antij requester register-role --platform github --username alice --repository yourorg/yourrepo
```

> Registering a role is not enough to gain rights to request test-runs. Your repository have to be white-listed by the agent. This requires you to get in contact with the agent and ask them to white-list your repository.

### Unregistering a role

```bash
antij requester unregister-role --platform github --username alice --repository yourorg/yourrepo
```

## Test-runs


### Setup

Once you are registered as a user and a role, you can request test-runs.

Before doing that make sure you have a commit in your repository containing a directory with "valid" test assets inside.

You can obtain a set of standard test assets by running the following command:

If you are in your repository directory, you can run:

```bash
antij requester generate-assets -D ./path/to/your/test/directory
```

Once you modified them you can try to run them locally  with the `antij requester test-run` command.

```bash
antij agent test-run -D ./path/to/your/test/directory
```

Then commit and push the changes to your repository.

### Requesting a test-run

Before proceding be careful to set the necessary signing assets in your environment variables.
```bash
  Which key selector to use from the SSH file
  env: ANTI_SSH_KEY_SELECTOR STRING

  Path to the SSH private key file
  env: ANTI_SSH_FILE FILEPATH
```

As with the wallet passphrase you can set the password in the environment variable

```bash
read -s -p "Enter password to decrypt the SSH private key: " ANTI_INTERACTIVE_PASSWORD
export ANTI_INTERACTIVE_PASSWORD
```

Or better paste it from a password manager each time you need it using the 'ask-password' option

Or set the `ANTI_INTERACTIVE_PASSWORD` environment variable to any value.

> The file at ANTI_SSH_FILE path has to be the encrypted ssh private key matching the user registration [see above](#registering-a-user-public-key).

To request a test-run, you can use the `antij requester create-test` command.

```bash
antij requester create-test --platform github --username alice --repository yourorg/yourrepo --directory ./path/to/your/test/directory --commit your_commit_hash --try 1 --duration 2
```

You can request multiple test-runs for the same commit but you have to specify a different `--try` number for each request.

### Checking the test-run status

You can check the status of your test-run requests with the `antij facts test-run` command.

```bash
antij facts test-run pending
```
