inThisBuild(
  List(
    organization := "co.topl",
    homepage := Some(url("https://github.com/Topl/BramblSc")),
    licenses := Seq("MPL2.0" -> url("https://www.mozilla.org/en-US/MPL/2.0/")),
    scalaVersion := "2.13.10",
    testFrameworks += TestFrameworks.MUnit
  )
)

lazy val commonScalacOptions = Seq(
  "-deprecation",
  "-feature",
  "-language:higherKinds",
  "-language:postfixOps",
  "-unchecked",
  "-Ywarn-unused:-implicits,-privates",
  "-Yrangepos"
)

lazy val commonSettings = Seq(
  scalacOptions ++= commonScalacOptions,
  semanticdbEnabled := true, // enable SemanticDB for Scalafix
  Compile / unmanagedSourceDirectories += {
    val sourceDir = (Compile / sourceDirectory).value
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, n)) if n >= 13 => sourceDir / "scala-2.13+"
      case _                       => sourceDir / "scala-2.12-"
    }
  },
  Test / testOptions ++= Seq(
    Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "2"),
    Tests.Argument(TestFrameworks.ScalaTest, "-f", "sbttest.log", "-oDGG", "-u", "target/test-reports")
  ),
  resolvers ++= Seq(
    "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/",
    "Sonatype Staging" at "https://s01.oss.sonatype.org/content/repositories/staging",
    "Sonatype Snapshots" at "https://s01.oss.sonatype.org/content/repositories/snapshots/",
    "Bintray" at "https://jcenter.bintray.com/",
    "jitpack" at "https://jitpack.io"
  ),
  addCompilerPlugin("org.typelevel" % "kind-projector"     % "0.13.2" cross CrossVersion.full),
  addCompilerPlugin("com.olegpy"   %% "better-monadic-for" % "0.3.1"),
  testFrameworks += TestFrameworks.MUnit
)

lazy val publishSettings = Seq(
  homepage := Some(url("https://github.com/Topl/BramblSc")),
  sonatypeCredentialHost := "s01.oss.sonatype.org",
  sonatypeRepository := "https://s01.oss.sonatype.org/service/local",
  Test / publishArtifact := false,
  pomIncludeRepository := { _ => false },
  pomExtra :=
    <developers>
      <developer>
        <id>scasplte2</id>
        <name>James Aman</name>
      </developer>
      <developer>
        <id>mgrand</id>
        <name>Mark Grand</name>
      </developer>
    </developers>,
)

lazy val scalamacrosParadiseSettings =
  Seq(
    scalacOptions ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, v)) if v >= 13 =>
          Seq(
            "-Ymacro-annotations"
          )
        case _ =>
          Nil
      }
    }
  )

lazy val typeclasses: Project = project
  .in(file("typeclasses"))
  .disablePlugins(AssemblyPlugin)
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "typeclasses",
    commonSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "co.topl.buildinfo.typeclasses"
  )
  .settings(
    libraryDependencies ++= Dependencies.logging
  )

lazy val crypto = project
  .in(file("crypto"))
  .settings(
    name := "crypto",
    commonSettings,
    publishSettings,
    libraryDependencies ++= Dependencies.crypto,
    scalamacrosParadiseSettings
  )

lazy val bramblSdk = project
  .in(file("brambl-sdk"))
  .settings(
    name := "brambl-sdk",
    commonSettings,
    publishSettings,
    libraryDependencies ++= Seq(Dependencies.protobufSpecs, Dependencies.quivr4s),
    scalamacrosParadiseSettings
  )
  .dependsOn(crypto)

lazy val brambl = project
  .in(file("."))
  .settings(
    moduleName := "brambl",
    commonSettings,
    publish / skip := true,
    libraryDependencies ++= Seq(Dependencies.protobufSpecs, Dependencies.quivr4s)
  )
  .enablePlugins(ReproducibleBuildsPlugin)
  .aggregate(
    crypto,
    bramblSdk
  )

addCommandAlias("checkPR", s"; scalafixAll --check; scalafmtCheckAll; +test")
addCommandAlias("preparePR", s"; scalafixAll; scalafmtAll; +test")
addCommandAlias("checkPRTestQuick", s"; scalafixAll --check; scalafmtCheckAll; testQuick")
