import Dependencies._

ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.3.0"

lazy val root = (project in file("."))
  .settings(
    name := "quantum-computing-functional",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.17" % Test,
      "org.typelevel" %% "cats-core" % "2.10.0",
      "org.typelevel" %% "spire" % "0.18.0"
    ),
    javacOptions ++= Seq("--release", "17"),
    scalacOptions ++= Seq("-java-output-version", "17")
  )