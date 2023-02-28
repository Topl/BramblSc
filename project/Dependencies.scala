import Dependencies.Versions._
import sbt._

object Dependencies {

  object Versions {
    val catsCoreVersion = "2.9.0"
    val simulacrumVersion = "1.0.1"
    val circeVersion = "0.14.4"
    val quivr4sVersion = "ef14fe2" // scala-steward:off
    val protobufSpecsVersion = "e08d4b2" // scala-steward:off
    val mUnitTeVersion = "0.7.29"
  }

  val catsSlf4j: ModuleID =
    "org.typelevel" %% "log4cats-slf4j" % "2.4.0"

  val circe: Seq[ModuleID] = Seq(
    "io.circe" %% "circe-core"    % circeVersion,
    "io.circe" %% "circe-parser"  % circeVersion,
    "io.circe" %% "circe-generic" % circeVersion
  )

  val scalacheck: Seq[ModuleID] = Seq(
    "org.scalacheck"    %% "scalacheck"      % "1.17.0",
    "org.scalatestplus" %% "scalacheck-1-14" % "3.2.2.0"
  )

  val scalamock: Seq[ModuleID] = Seq(
    "org.scalamock" %% "scalamock" % "5.2.0"
  )

  val scalatest: Seq[ModuleID] = Seq(
    "org.scalatest"    %% "scalatest"                     % "3.2.15",
    "com.ironcorelabs" %% "cats-scalatest"                % "3.1.1",
    "org.typelevel"    %% "cats-effect-testing-scalatest" % "1.5.0"
  )

  val mUnitTest: Seq[ModuleID] = Seq(
    "org.scalameta" %% "munit"                   % mUnitTeVersion,
    "org.scalameta" %% "munit-scalacheck"        % mUnitTeVersion,
    "org.typelevel" %% "munit-cats-effect-3"     % "1.0.7",
    "org.typelevel" %% "scalacheck-effect-munit" % "1.0.4"
  )

  val newType: Seq[ModuleID] = Seq(
    "io.estatico" %% "newtype" % "0.4.4"
  )

  val cats: Seq[ModuleID] = Seq(
    "org.typelevel" %% "cats-core" % catsCoreVersion,
    "org.typelevel" %% "mouse"     % "1.2.1"
  )

  val simulacrum: Seq[ModuleID] = Seq(
    "org.typelevel" %% "simulacrum" % simulacrumVersion
  )

  val scodec: Seq[ModuleID] = Seq(
    "org.scodec" %% "scodec-core" % "1.11.10",
    "org.scodec" %% "scodec-bits" % "1.1.36",
    "org.scodec" %% "scodec-cats" % "1.2.0"
  )

  val protobufSpecs: Seq[ModuleID] = Seq(
    "com.github.Topl" % "protobuf-specs" % protobufSpecsVersion
  )

  val quivr4s: Seq[ModuleID] = Seq(
    "com.github.Topl" % "quivr4s" % quivr4sVersion
  )

  object Crypto {

    lazy val sources: Seq[ModuleID] =
      Seq("org.bouncycastle" % "bcprov-jdk18on" % "1.72") ++
      scodec ++
      newType ++
      cats ++
      simulacrum

    lazy val tests: Seq[ModuleID] =
      (
        circe ++
          scalatest ++
          scalamock ++
          scalacheck
      )
        .map(_ % Test)
  }

  object BramblSdk {

    lazy val sources: Seq[ModuleID] =
      quivr4s

    lazy val tests: Seq[ModuleID] =
      (
        quivr4s.map(_ classifier ("tests")) ++
          mUnitTest ++
          scalamock
      ).map(_ % Test)
  }
}
