---
sidebar_position: 1
---

# The Topl Ecosystem

The Topl Ecosystem is a collection of tools and services that enable the creation of a Topl Network. The Topl Ecosystem is comprised of the following components:

- The Bifrost Node
- The Genus Node
- The brambl-cli
- The various brambl-sdks
- The annulus block explorer
- The various networks

## The Bifrost Node

The Bifrost Node is the core of the Topl Ecosystem. It is a node that runs on the Topl Network and is responsible for the following:

- Maintaining the Topl Network's ledger
- Maintaining consensus with other nodes on the Topl Network using the proof-of-stake consensus algorithm.

## The Genus Node

The Genus Node runs as part of the Bifrost Node, but it is responsible for the following aggregating transactions from the Topl to make querying easier for clients.

## The brambl-cli

The brambl-cli is a command line interface that allows users to interact with the Topl Network. 
I can perform the following actions:

- Create a new wallet
- Transfer tokens
- Query the Topl Network (Both Genus and Bifrost Nodes)
- Mint new tokens

## The brambl-sdks

The brambl-sdks are a collection of software development kits that allow developers to interact with the Topl Network. There are currently SDKs for the following languages:

- Scala (BramblSc)
- Dart (BramblDart)

## The annulus block explorer

The annulus block explorer is a web application that allows users to view the contents of the Topl Network's ledger. The annulus block explorer is hosted at https://explore.topl.co/.

## The various networks

The Topl Ecosystem has a number of test networks that allow developers to test their applications before deploying them to the main network. The test networks are as follows:

- The local network
- The public test network
- The main network

The local network is a network that runs on a developer's local machine. It is useful for testing applications before deploying them to the public test network. The public test network is a network that runs on the public internet. It is useful for testing applications before deploying them to the main network. 

Developers can start a local network by running the following command:

```bash
docker run --rm -p 9085:9085 -p 9084:9084 -p 9091:9091 docker.io/toplprotocol/bifrost-node:2.0.0-alpha10
```

The above command will start a local network on the developer's machine. The developer can then interact with the local network using the brambl-cli or the brambl-sdks.

The public test and main network is hosted on the public internet. The addresses for the public test network are as follows:

- Bifrost and Genus Node: testnet.topl.co:443 

The addresses for the main network are as follows:

- Bifrost and Genus Node: mainnet.topl.co:443

