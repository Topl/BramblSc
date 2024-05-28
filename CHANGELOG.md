# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased] - YYYY-MM-DD - TODO replace date after release 

### Added

- Added a height field to the blocks in the Bifrost monitor stream

## [v2.0.0-beta6] - 2024-05-22

### Changed

- Fixed responsiveness warning: Bifrost monitor uses fromBlockingIterator
- Added height field to the Bitcoin Monitor Stream
- Added support for reorgs to the Bitcoin Monitor Stream. The stream sends back blocks to be either Applied or Unapplied.

## [v2.0.0-beta5] - 2024-05-08

### Added

- Bifrost network monitoring. This change allows the ability to monitor a bifrost network for applied and unapplied blocks, returning a stream.
- Added a height field to the blocks in the Bitcoin monitor stream

## [v2.0.0-beta4] - 2024-04-23

### Added

- Bitcoin network monitoring. This change allows the ability to monitor a bitcoin network for new blocks, returning a stream.

## [v2.0.0-beta3] - 2024-03-04

### Added

- Support for Sha256 Digest Propositions in the SDK. This change allows successful Quivr validation of a Digest Proposition with routine="Sha256".

### Changed

- Use Async[F].cede to prevent starvation

## [v2.0.0-beta2] - 2024-01-10

### Added

- Added a tutorial for creating a new custom asset
- Pretty print (display) capabilities for the IoTransaction model
- Pretty print (display) capabilities for Quivr Propositions, Proofs, and validation errors.

### Changed

- Service Kit's initWalletState now expects a Topl main key pair instead of a (1,1) partial derivative verification key

## [v2.0.0-beta1] - 2023-12-05

### Added

- Added documentation for using a wallet (extract main key, derive child keys) 
- Added documentation wallet functionality using persistence (createAndSaveNewWallet, importWalletAndSave, loadAndExtractMainKey) 
- Added documentation for updating a wallet state
- Added tutorials: Load new wallet with Genesis funds, Transfer funds from one wallet to another
- Support saving and retrieving the preimage for Digest Propositions

## [v2.0.0-alpha4-SNAPSHOT] 

### Added
- TODO replace items after release

## [Released] 

## [v2.0.0-alpha3] - Tue Aug 01 19:17:07 UTC 2023	

### Changed
 
- It includes the following artifacts: brambl-sdk, crypto, service-kit (new artifact!)  (#87) @mundacho
- Scala Service Kit Implementation (#84) @DiademShoukralla
- Set up Scala Service Kit Project (#82) @Diadem Shoukralla

## [v2.0.0-alpha2] - Mon Jul 17 20:15:02 UTC 2023

### Changed

- Update quivr4s to 2.0.0-alpha2 (#80) @mundacho
- Dependency updates (#79) @scala-steward
- Updated README.md with updated consumption instructions (#78) @DiademShoukralla
- TSDK-507 Release drafts should not contain changes from previous pre-release (#77) @DiademShoukralla
- Dependency updates (#76) @scala-steward
- TSDK-530 Minor fixes to Locked and Digest Proposition Templates (#75) @DiademShoukralla
- Dependency updates in main (#69) @scala-steward
- update sbt version 1.9.1 (#74) @nandotorterolo
- Update TransactionSyntaxInterpreter.scala (#72) @nandotorterolo


## [v2.0.0-alpha1] - Fri Jun 30 20:11:18 UTC 2023		

### Added

- First release was included on Sonatype [releases](https://s01.oss.sonatype.org/content/repositories/releases/co/topl/brambl-sdk_2.13/) repository.
