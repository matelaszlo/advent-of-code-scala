lazy val root = (project in file("."))
  .settings(name := "Advent of Code Scala")
  .settings(moduleName := "advent-of-code-scala")
  .settings(version := "0.1")
  .settings(scalaVersion := "2.13.0")
  .settings(libraryDependencies ++= dependencies)

lazy val dependencies = Seq("org.scalatest" %% "scalatest"   % "3.0.8"  % "test")
