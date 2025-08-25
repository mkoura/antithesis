# Code Design Decisions

##  opt-env-conf vs optparse-applicative

Optparse-applicative is a popular library for parsing command-line options in Haskell. It provides a declarative way to define command-line interfaces and supports features like subcommands, flags, and arguments.

Opt-env-conf is a library that extends optparse-applicative by adding support for environment variables and configuration files. It allows developers to define options that can be set via command-line arguments, environment variables, or configuration files, providing more flexibility in how applications can be configured.

### Decision

We chose to use opt-env-conf over optparse-applicative for the following reasons:
1. Some commands require some env vars to be set, while others don't. opt-env-conf allows us to handle this complexity more gracefully, wihout passing around Maybe values.
2. We need to handle both encrypted and unencrypted mnemonics. Depending on that the settings are different. opt-env-conf allows us to define different settings based on the context, making it easier to manage these variations. In particular after parsing the env var ANTI_WALLET_FILE we insert a read-only operation (checkMapIO) that brings in the fields of the wallet file as settings. Depending on the field we require different settings for the passphrase.
3. We want to support interactive prompts for sensitive information like passwords. opt-env-conf `mapIO` allows us to introduce interactive prompts as part of the settings parsing process, making it easier to handle user input securely.