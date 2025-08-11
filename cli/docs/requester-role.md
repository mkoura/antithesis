# Requester role

This is the role of the user who wants to run tests using the Antithesis platform.

Before you can request a test run, you need to register yourself as github user with an ed25519 public key.

## Registrations

A couple of requests have to be made once before the regular test-run requests can succeed. These requests are used to register the user and the project on the Antithesis platform.

ATM we only support Github as a platform, but we plan to support other platforms in the future.

Registration and unregistrations can be requested by anyone. The oracle role will simply validate the facts against the Github platform and update the Antithesis token accordingly. You cannot register a user or project that is already registered.

Be careful that there is no imperativity here, so i.e. you cannot unregister a user public key if it's present in Github.

### Registering a user public key

To register yourself as a user, you can use the `anti requester register-user` command.

```bash
anti requester register-user --platform github --username alice --pubkeyhash AAAAC3NzaC1lZDI1NTE5AAAAIO773JHqlyLm5XzOjSe+Q5yFJyLFuMLL6+n63t4t7HR8
```

As with all other requests, once submitted regularly you have to wait for the oracle to merge your request into the Antithesis token.

You can use the `anti oracle token get` command to inspect your pending requests in the Antithesis token.

You can use the `anti facts` command to query the Antithesis token and see if your user is part of the facts.

```bash
anti oracle token get | jq '.result.requests' "
```

Until your requests is there, you cannot proceed with the next steps.

As with all requests to an mpfs you can retract your request using the `anti retract` command, anytime before the oracle merges it into the Antithesis token.

Get the `outputRefId` of your request from pending requests command output and use it to retract your request

```bash
anti retract -o 9ec36572e01bca9f7d32d791a5a6c529ef91c10d536f662735af26311b2c8766-0
```
ATM the oracle is not able to justify a request rejection. But anti cli will apply the oracle validation before submitting it, so rejections will be caught before submitting the request.

### Unregistering a user public key

To unregister a user, you can use the `anti requester unregister-user` command.

```bash
anti requester unregister-user --platform github --username alice --pubkeyhash AAAAC3NzaC1lZDI1NTE5AAAAIO773JHqlyLm5XzOjSe+Q5yFJyLFuMLL6+n63t4t7HR8
```



