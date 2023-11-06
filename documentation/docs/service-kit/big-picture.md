---
sidebar_position: 2
---

# Overview of Service Kit

The service kit is an opinionated implementation of several APIs defined
in the brambl-sdk. It is designed to be a simple way to get started. It provides
implementations for:

- `FellowshipStorageAlgebra`
- `TemplateStorageAlgebra`
- `WalletKeyApiAlgebra`
- `WalletStateAlgebra`

## The Default Implementation

Working with the chain requires a wallet. The default implementation of the
wallet implements HD Key derivation. This requires to keep track of the
wallet state. 

The wallet, thus, has two parts:

- The wallet vault
- The wallet state

## The Wallet Vault

The wallet vault is the encrypted master key. The `brambl-sdk` does not impose
any restrictions on how the vault is stored. The service kit provides a simple
implementation that stores the vault in an encrypted file. The functions to 
interact with the wallet vault are defined in the `WalletKeyApiAlgebra` trait. 

## The Wallet State

The wallet state keeps track of the different fellowships, templates and
interactions that the wallet has. The `brambl-sdk` does not impose any
restrictions on how the state is stored. The service kit provides a simple
implementation that stores the state in a sqlite database file.

The functions to interact with the wallet state are defined in the 
`FellowshipStorageAlgebra`, `TemplateStorageAlgebra` and `WalletStateAlgebra`.

### The Default Fellowship Storage

The default fellowship storage is a database table with the following schema:

```sql
CREATE TABLE IF NOT EXISTS fellowships (fellowship TEXT, x_fellowship INTEGER PRIMARY KEY ASC);
CREATE INDEX IF NOT EXISTS fellowship_names_idx ON fellowships (fellowship);
```

The `fellowship` column is the identifier of the fellowship. The `x_fellowship`
column is the index of the fellowship in the HD wallet. By default, the
wallet initialization procedure creates two fellowships:

```sql
INSERT INTO fellowships (fellowship, x_fellowship) VALUES ('nofellowship', 0);
INSERT INTO fellowships (fellowship, x_fellowship) VALUES ('self', 1);
```

The `nofellowship` fellowship is used for templates that don't have fellows,
for example, a height lock with no signature would be attached to the
`nofellowship` fellowship. The `self` fellowship is used for contracts where the
only participant is the owner of the wallet.

### The Default Template Storage

The default template storage is a database table with the following schema:

```sql
CREATE TABLE IF NOT EXISTS templates (template TEXT NOT NULL, y_template INTEGER PRIMARY KEY ASC,  lock TEXT NOT NULL);
CREATE UNIQUE INDEX IF NOT EXISTS template_names_idx ON templates (template);
```

The `template` column is the identifier of the template. The `y_template`
column is the index of the template in the HD wallet. The `lock` column is the
lock script of the template. By default, the wallet initialization procedure
creates two template:

```sql
INSERT INTO templates (template, y_template, lock) VALUES ('default', 1, '<encoded as JSON threshold(1, sign(0))>');
INSERT INTO templates (template, y_template, lock) VALUES ('genesis', 2, '<encoded as JSON threshold(1, height(1, MAXLONG)>')
```

### The Default Wallet State

The default wallet state, i.e. is a database table with the following schema:

```sql
CREATE TABLE IF NOT EXISTS cartesian (id INTEGER PRIMARY KEY, x_fellowship INTEGER NOT NULL, y_template INTEGER NOT NULL, z_interaction INTEGER NOT NULL, lock_predicate TEXT NOT NULL, address TEXT NOT NULL, routine TEXT, vk TEXT);
CREATE UNIQUE INDEX IF NOT EXISTS cartesian_coordinates ON cartesian (x_fellowship, y_template, z_interaction);
```

The `x_fellowship`, `y_template` and `z_interaction` columns are the coordinates
of the interaction in the HD wallet. The `lock_predicate` column is the lock
script of the interaction. The `address` column is the address of the
interaction. The `routine` column is the routine of the interaction, currently
`ExtendedEd25519`. The `vk` is the verification key of the interaction.
The verification key allows us to recover the coordinates of the interaction
from the `vk`, which is available in the transaction.
