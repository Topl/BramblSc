![GitHub tag (latest SemVer)](https://img.shields.io/github/v/tag/topl/bramblsc?label=release&style=plastic)

# BramblSc

Topl's Brambl SDK implemented in Scala

Multiple artifacts will be built from this repo. Some will be just for Topl clients and some will be shared. 

## Consume with JitPack

This repo can be consumed using jitpack. Here is how:

First, be sure to add jitpack to the end of the resolvers list in build.sbt. It should look like this:
```sbt
  resolvers ++= Seq(
    "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/",
    "Sonatype Staging" at "https://s01.oss.sonatype.org/content/repositories/staging",
    "Sonatype Snapshots" at "https://s01.oss.sonatype.org/content/repositories/snapshots/",
    "Bintray" at "https://jcenter.bintray.com/",
    "jitpack" at "https://jitpack.io"
  )
```

Then just add the dependency like this:
```sbt
  val bramblSc =
    "com.github.Topl" % "BramblSc" % "1bdc895"
```
Where `1bdc895` refers to a commit on this repo's main branch. This will add the artifacts for both `brambl-sdk` and `crypto`.
Then just use the dependencies like you would any other.

## Consume Maven Release

First, be sure to add Sonatype s01 releases to the end of the resolvers list in build.sbt. It should look like this:
```sbt
  resolvers ++= Seq(
    "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/",
    "Sonatype Staging" at "https://s01.oss.sonatype.org/content/repositories/staging",
    "Sonatype Snapshots" at "https://s01.oss.sonatype.org/content/repositories/snapshots/",
    "Bintray" at "https://jcenter.bintray.com/",
    "jitpack" at "https://jitpack.io",
    "Sonatype Releases" at "https://s01.oss.sonatype.org/content/repositories/releases/"
  )
```

Then just add the dependencies for `brambl-sdk` and `crypto` like this:
```sbt
  val brambl-sdk =
    "co.topl" %% "brambl-sdk" % "2.0.0-alpha1"
```

```sbt
  val crypto =
    "co.topl" %% "crypto" % "2.0.0-alpha1"
```

Replace `2.0.0-alpha1` with the latest released version. Then just use the dependencies like you would any other.