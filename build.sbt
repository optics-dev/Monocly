val dottyVersion = "0.22.0-bin-20200123-9982f0d-NIGHTLY"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Monocly",
    version := "0.1.0",

    scalaVersion := dottyVersion,
    resolvers ++= Seq(
      Resolver.sonatypeRepo("releases"),
      Resolver.sonatypeRepo("snapshots")
    ),

    libraryDependencies += "dev.travisbrown" %% "scalatest" % ("3.1.0" + "-20200123-9982f0d-NIGHTLY")
  )
