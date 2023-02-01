import sbt._

object Dependencies {
  val catsCoreVersion = "2.8.0"
  val simulacrumVersion = "1.0.1"
  val circeVersion = "0.14.3"

  val catsSlf4j: ModuleID =
    "org.typelevel" %% "log4cats-slf4j" % "2.4.0"

  val logging: Seq[ModuleID] = Seq(
    "com.typesafe.scala-logging" %% "scala-logging"   % "3.9.5",
    "ch.qos.logback"              % "logback-classic" % "1.2.11",
    "ch.qos.logback"              % "logback-core"    % "1.2.11",
    "org.slf4j"                   % "slf4j-api"       % "1.7.36",
    catsSlf4j
  )

  val circe: Seq[ModuleID] = Seq(
    "io.circe" %% "circe-core"    % circeVersion % "test",
    "io.circe" %% "circe-parser"  % circeVersion % "test",
    "io.circe" %% "circe-generic" % circeVersion % "test"
  )

  val scalacheck: Seq[ModuleID] = Seq(
    "org.scalacheck"    %% "scalacheck"      % "1.17.0"  % "test",
    "org.scalatestplus" %% "scalacheck-1-14" % "3.2.2.0" % "test"
  )

  val scalamock: Seq[ModuleID] = Seq(
    "org.scalamock" %% "scalamock" % "5.2.0" % "test"
  )

  val test: Seq[ModuleID] = Seq(
    "org.scalatest"    %% "scalatest"                     % "3.2.14" % "test",
    "com.ironcorelabs" %% "cats-scalatest"                % "3.1.1"  % "test",
    "org.typelevel"    %% "cats-effect-testing-scalatest" % "1.4.0"  % "test"
  ) ++ scalacheck ++ scalamock

  val mUnitTest: Seq[ModuleID] = Seq(
    "org.scalameta" %% "munit"                   % "0.7.29" % Test,
    "org.scalameta" %% "munit-scalacheck"        % "0.7.29" % Test,
    "org.typelevel" %% "munit-cats-effect-3"     % "1.0.7"  % Test,
    "org.typelevel" %% "scalacheck-effect-munit" % "1.0.4"  % Test
  ) ++ scalamock

  val newType: Seq[ModuleID] = Seq(
    "io.estatico" %% "newtype" % "0.4.4"
  )

  val cats: Seq[ModuleID] = Seq(
    "org.typelevel" %% "cats-core" % catsCoreVersion,
    "org.typelevel" %% "mouse"     % "1.1.0"
  )

  val simulacrum: Seq[ModuleID] = Seq(
    "org.typelevel" %% "simulacrum" % simulacrumVersion
  )

  val scodec: Seq[ModuleID] = Seq(
    "org.scodec" %% "scodec-core" % "1.11.9",
    "org.scodec" %% "scodec-bits" % "1.1.34",
    "org.scodec" %% "scodec-cats" % "1.1.0"
  )

  val protobufSpecs: ModuleID = "com.github.Topl" % "protobuf-specs" % "22abe91"

  val quivr4s: ModuleID = "com.github.Topl" % "quivr4s" % "27c502e"

  lazy val crypto: Seq[ModuleID] =
    Seq("org.bouncycastle" % "bcprov-jdk18on" % "1.72") ++
    scodec ++
    newType ++
    cats ++
    circe ++
    simulacrum ++
    test
}
