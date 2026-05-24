val zioVersion = "2.1.26"

lazy val root = project
  .in(file("."))
  .settings(
    name := "scala-crypto-kata",
    organization := "crypto.kata",
    version := "0.0.1",
    scalaVersion := "3.7.3",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio"               % zioVersion,
      "dev.zio" %% "zio-test"          % zioVersion % Test,
      "dev.zio" %% "zio-test-sbt"      % zioVersion % Test,
      "dev.zio" %% "zio-test-junit"    % zioVersion % Test,
      "dev.zio" %% "zio-test-magnolia" % zioVersion % Test
    ),
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))
  )
