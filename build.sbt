lazy val root = (project in file("."))
  .settings(name := "Advent of Code Scala")
  .settings(moduleName := "advent-of-code-scala")
  .settings(version := "1.0")
  .settings(scalaVersion := "2.13.7")
  .settings(libraryDependencies ++= dependencies)

lazy val dependencies = Seq(
  "org.scala-lang"  % "scala-reflect" % "2.13.7",
  "org.scalatest"  %% "scalatest"     % "3.2.10"  % "test"
)

scalacOptions ++= Seq("-deprecation", "-feature")
