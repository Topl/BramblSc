
Seq(
  "com.eed3si9n"            % "sbt-assembly"              % "2.1.1",
//  "org.scalastyle"         %% "scalastyle-sbt-plugin"     % "1.0.0",
  "org.scoverage"           % "sbt-scoverage"             % "2.0.9",
  "com.github.sbt"          % "sbt-release"               % "1.1.0",
//  "io.kamon"                % "sbt-kanela-runner"         % "2.0.14",
//  "com.github.cb372"        % "sbt-explicit-dependencies" % "0.2.16",
//  "pl.project13.scala"      % "sbt-jmh"                   % "0.4.3",
  "org.scalameta"           % "sbt-scalafmt"              % "2.5.2",
  "ch.epfl.scala"           % "sbt-scalafix"              % "0.11.0",
//  "org.wartremover"         % "sbt-wartremover"           % "3.0.7",
  "com.eed3si9n"            % "sbt-buildinfo"             % "0.11.0",
  "com.github.sbt"          % "sbt-ci-release"            % "1.5.12",
  "net.bzzt"                % "sbt-reproducible-builds"   % "0.31",
).map(addSbtPlugin)
