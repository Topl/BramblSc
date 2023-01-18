![GitHub tag (latest SemVer)](https://img.shields.io/github/v/tag/topl/bramblsc?label=release&sort=semver&style=plastic)

# BramblSc

Topl's Brambl SDK implemented in Scala

Multiple artifacts will be built from this repo. Some will be just for Topl clients and some will be shared. Currently 

This repo should be consumed using jitpack. Here is how:

First, be sure to add jitpack to the end of the resolvers list in build.sbt. It should look like this:
```sbt
  resolvers ++= Seq(
    "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/",
    "Sonatype Staging" at "https://s01.oss.sonatype.org/content/repositories/staging",
    "Sonatype Snapshots" at "https://s01.oss.sonatype.org/content/repositories/snapshots/",
    "Bintray" at "https://jcenter.bintray.com/",
    "jitpack" at "https://jitpack.io"
```

Then just add the dependency like this:
```sbt
  val bramblScCrypto =
    "com.github.Topl" % "BramblSc" % "v2.0.3"
```

Replace the release in the example with the latest release. Then just use the dependency like you would any other.
