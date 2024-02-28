
Seq(
  "com.eed3si9n"            % "sbt-assembly"              % "2.1.5",
  "org.scoverage"           % "sbt-scoverage"             % "2.0.9",
  "com.github.sbt"          % "sbt-release"               % "1.4.0",
  "org.scalameta"           % "sbt-scalafmt"              % "2.5.2",
  "ch.epfl.scala"           % "sbt-scalafix"              % "0.11.1",
  "com.eed3si9n"            % "sbt-buildinfo"             % "0.11.0",
  "com.github.sbt"          % "sbt-ci-release"            % "1.5.12",
  "net.bzzt"                % "sbt-reproducible-builds"   % "0.31",
  "com.github.sbt"          % "sbt-unidoc"                % "0.5.0",
).map(addSbtPlugin)
