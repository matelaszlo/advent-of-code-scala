lazy val root = (project in file("."))
  .settings(name := "Advent of Code Scala")
  .settings(moduleName := "advent-of-code-scala")
  .settings(version := "0.1")
  .settings(scalaVersion := "2.12.8")
  .settings(libraryDependencies ++= dependencies)

lazy val dependencies = Seq("org.scalatest" %% "scalatest"   % "3.0.5"  % "test")
