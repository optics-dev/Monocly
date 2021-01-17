lazy val root = project
  .in(file("."))
  .settings(
    name := "Monocly",
    version := "0.1.0",
    scalacOptions += "-language:implicitConversions",
    scalaVersion := "3.0.0-M3",
    useScala3doc := true,
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.20" % "test",
    testFrameworks += new TestFramework("munit.Framework")
  )
