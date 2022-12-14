import sbt._

object Dependencies {

  val catsSlf4j: ModuleID =
    "org.typelevel" %% "log4cats-slf4j" % "2.4.0"

  val logging: Seq[ModuleID] = Seq(
    "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
    "ch.qos.logback" % "logback-classic" % "1.2.11",
    "ch.qos.logback" % "logback-core" % "1.2.11",
    "org.slf4j" % "slf4j-api" % "1.7.36",
    catsSlf4j
  )

  val scalamock: Seq[ModuleID] = Seq(
    "org.scalamock" %% "scalamock" % "5.2.0" % "test"
  )

val mUnitTest: Seq[ModuleID] = Seq(
  "org.scalameta" %% "munit" % "0.7.29" % Test,
  "org.scalameta" %% "munit-scalacheck" % "0.7.29" % Test,
  "org.typelevel" %% "munit-cats-effect-3" % "1.0.7" % Test,
  "org.typelevel" %% "scalacheck-effect-munit" % "1.0.4" % Test
) ++ scalamock

//  lazy val crypto: Seq[ModuleID] =
  //    scodec ++
  //      newType ++
  //      circe ++
  //      externalCrypto ++
  //      cats ++
  //      simulacrum ++
  //  test
}
