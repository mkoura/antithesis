# Oracle role

This is the role of the user that wants to run an oracle service for the Antithesis platform. There will be only one token and so there will be only one oracle service running at a time, but we document it here for completeness.

## Creating the anti token (only for testing)

To create the Antithesis token, you can use the `anti oracle token create` command.

```bash
anti oracle token boot
```

It will create the Antithesis token. This token is a unique identifier for the Antithesis platform and will be used by all users to interact with the platform. You have to distribute it so that users can set the `ANTI_TOKEN_ID` environment variable to point to it.

To test the role change the ANTI_TOKEN_ID environment variable to the new create token id

You can review the token info anytime with


```bash
anti oracle token get | jq '.result'
```
## Validating requests

You can use the validate command to automate the process of oracling the requests

```bash
anti oracle requests validate
```

## Updating the anti token

Once you decided what to include in the Antithesis token, you can commit the requests to the token.

Updating the token with new requests is done with the `anti oracle token update` command. As with retract you have to provide the `outputRefId` of the request you want to update. Multiple requests can be updated at once, so you can provide multiple `-o` options.

```bash
anti oracle token update -o b6fc7cca5bcae74e6a5983f7922d0e0985285f1f19e62ccc9cb9fd4d3766a81b-0
```

## Deleting the anti token (only for testing)

To delete the Antithesis token, you can use the `anti oracle token delete` command.

```bash
anti oracle token delete
```