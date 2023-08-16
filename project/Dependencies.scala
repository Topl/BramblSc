import Dependencies.Versions._
import sbt._

object Dependencies {

  object Versions {
    val catsCoreVersion = "2.9.0"
    val simulacrumVersion = "1.0.1"
    val circeVersion = "0.14.5"
    val protobufSpecsVersion = "2.0.0-alpha3"
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
    "org.scalatestplus" %% "scalacheck-1-16" % "3.2.14.0"
  )

  val scalamock: Seq[ModuleID] = Seq(
    "org.scalamock" %% "scalamock" % "5.2.0"
  )

  val scalatest: Seq[ModuleID] = Seq(
    "org.scalatest"    %% "scalatest"                     % "3.2.16",
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
    "org.typelevel" %% "cats-core"   % catsCoreVersion,
    "org.typelevel" %% "mouse"       % "1.2.1",
    "org.typelevel" %% "cats-free"   % catsCoreVersion,
    "org.typelevel" %% "cats-effect" % "3.4.11"
  )

  val simulacrum: Seq[ModuleID] = Seq(
    "org.typelevel" %% "simulacrum" % simulacrumVersion
  )

  val protobufSpecs: Seq[ModuleID] = Seq(
    "co.topl" %% "protobuf-fs2" % protobufSpecsVersion
  )

  val sqlite: Seq[ModuleID] = Seq(
    "org.xerial" % "sqlite-jdbc" % "3.42.0.0"
  )

  object Crypto {

    lazy val sources: Seq[ModuleID] =
      Seq("org.bouncycastle" % "bcprov-jdk18on" % "1.76") ++
      circe ++
      newType ++
      cats ++
      simulacrum

    lazy val tests: Seq[ModuleID] =
      (
        scalatest ++
          scalamock ++
          scalacheck
      )
        .map(_ % Test)
  }

  object BramblSdk {

    lazy val sources: Seq[ModuleID] = Dependencies.protobufSpecs

    lazy val tests: Seq[ModuleID] =
      (
        mUnitTest ++
          scalamock
      ).map(_ % Test)
  }

  object ServiceKit {

    lazy val sources: Seq[ModuleID] = sqlite

    lazy val tests: Seq[ModuleID] = (
      mUnitTest ++ sqlite
    ).map(_ % Test)
  }

  object Quivr4s {

    lazy val sources: Seq[ModuleID] = Dependencies.protobufSpecs

    lazy val tests: Seq[ModuleID] = (
      mUnitTest
    ).map(_ % Test)
  }
}
