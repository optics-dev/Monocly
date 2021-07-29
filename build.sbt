lazy val root = project
  .in(file("."))
  .settings(
    name := "Monocly",
    version := "0.1.0",
    scalacOptions += "-language:implicitConversions",
    scalaVersion := "3.0.0",
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.27" % Test,
    testFrameworks += new TestFramework("munit.Framework")
  )
