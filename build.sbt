ThisBuild / scalaVersion     := "2.13.8"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "karma-macros",
    libraryDependencies ++= Seq(
      // TODO: Huh? Why Java?
      "org.scala-lang"                % "scala-reflect" % scalaVersion.value,
      "com.softwaremill.magnolia1_2" %% "magnolia"      % "1.1.2"
    ),
    scalacOptions ++= Seq(
      "-language:experimental.macros"
    )
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
