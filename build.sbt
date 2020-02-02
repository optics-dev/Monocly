val dottyVersion = "0.21.0-RC1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Monocly",
    version := "0.1.0",
    scalaVersion := dottyVersion,
    libraryDependencies += "org.scalameta" %% "munit" % "0.4.3" % "test",
    testFrameworks += new TestFramework("munit.Framework")
  )
