val scala213 = "2.13.11"
val scala33 = "3.3.0"

inThisBuild(
  List(
    organization := "co.topl",
    homepage := Some(url("https://github.com/Topl/BramblSc")),
    licenses := Seq("MPL2.0" -> url("https://www.mozilla.org/en-US/MPL/2.0/")),
    scalaVersion := scala213,
    testFrameworks += TestFrameworks.MUnit
  )
)

lazy val commonScalacOptions = Seq(
  "-deprecation",
  "-feature",
  "-language:higherKinds",
  "-language:postfixOps",
  "-unchecked"
)

lazy val commonSettings = Seq(
  fork := true,
  Compile / scalacOptions ++= commonScalacOptions,
  Compile / scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, v)) if v >= 13 =>
        Seq(
          "-Ywarn-unused:-implicits,-privates",
          "-Yrangepos"
        )
      case _ =>
        Nil
    }
  },
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
    "Sonatype Releases s01" at "https://s01.oss.sonatype.org/content/repositories/releases/",
    "Bintray" at "https://jcenter.bintray.com/",
    "jitpack" at "https://jitpack.io"
  ),
  libraryDependencies ++= {
    scalaVersion.value match {
      case `scala33` =>
        Nil
      case _ =>
        List(
          // be careful Note: for multi-project builds - put addCompilerPlugin clause into settings section for each sub-project.
          compilerPlugin("org.typelevel" % "kind-projector"     % "0.13.2" cross CrossVersion.full),
          compilerPlugin("com.olegpy"   %% "better-monadic-for" % "0.3.1")
        )
    }
  },
//  addCompilerPlugin("org.typelevel" % "kind-projector"     % "0.13.2" cross CrossVersion.full),
//  addCompilerPlugin("com.olegpy"   %% "better-monadic-for" % "0.3.1"),
  testFrameworks += TestFrameworks.MUnit
)

lazy val publishSettings = Seq(
  homepage := Some(url("https://github.com/Topl/BramblSc")),
  ThisBuild / sonatypeCredentialHost := "s01.oss.sonatype.org",
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
    </developers>
)

lazy val macroAnnotationsSettings =
  Seq(
    scalacOptions ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, v)) if v >= 13 =>
          Seq(
            "-Ymacro-annotations",
          )
        case _ =>
          Nil
      }
    }
  )

lazy val crypto = project
  .in(file("crypto"))
  .settings(
    name := "crypto",
    commonSettings,
    publishSettings,
    crossScalaVersions := Seq(scala213, scala33),
    Test / publishArtifact := true,
    libraryDependencies ++=
      Dependencies.Crypto.sources ++
      Dependencies.Crypto.tests(CrossVersion.partialVersion(Keys.scalaVersion.value)),
    macroAnnotationsSettings
  )

lazy val quivr4s = project
  .in(file("quivr4s"))
  .settings(
    name := "quivr4s",
    commonSettings,
    publishSettings,
    crossScalaVersions := Seq(scala213, scala33),
    Test / publishArtifact := true,
    Test / parallelExecution := false,
    libraryDependencies ++=
      Dependencies.Quivr4s.sources ++
      Dependencies.Quivr4s.tests
  )
  .dependsOn(crypto)

lazy val bramblSdk = project
  .in(file("brambl-sdk"))
  .settings(
    name := "brambl-sdk",
    commonSettings,
    publishSettings,
    crossScalaVersions := Seq(scala213, scala33),
    Test / publishArtifact := true,
    Test / parallelExecution := false,
    libraryDependencies ++=
      Dependencies.BramblSdk.sources ++
      Dependencies.BramblSdk.tests(CrossVersion.partialVersion(Keys.scalaVersion.value))
  )
  .dependsOn(quivr4s % "compile->compile;test->test")

lazy val serviceKit = project
  .in(file("service-kit"))
  .settings(
    name := "service-kit",
    commonSettings,
    publishSettings,
    crossScalaVersions := Seq(scala213), // issues with scala3, compile hangs
    Test / publishArtifact := true,
    Test / parallelExecution := false,
    libraryDependencies ++=
      Dependencies.ServiceKit.sources ++
      Dependencies.ServiceKit.tests
  )
  .dependsOn(bramblSdk)

lazy val brambl = project
  .in(file("."))
  .settings(
    moduleName := "brambl",
    commonSettings,
    // crossScalaVersions must be set to Nil on the aggregating project
    crossScalaVersions := Nil,
    publish / skip := true
  )
  .enablePlugins(ReproducibleBuildsPlugin)
  .aggregate(
    crypto,
    quivr4s,
    bramblSdk,
    serviceKit,
  )

addCommandAlias("checkPR", s"; scalafixAll --check; scalafmtCheckAll; +test")
addCommandAlias("preparePR", s"; scalafixAll; scalafmtAll; +test")
addCommandAlias("checkPRTestQuick", s"; scalafixAll --check; scalafmtCheckAll; +testQuick")
