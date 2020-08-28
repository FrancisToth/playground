ThisBuild / scalaVersion     := "2.13.2"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "dev.contramap"
ThisBuild / organizationName := "contramap"

lazy val root = (project in file("."))
  .settings(
    name := "playground"
  )

