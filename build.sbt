val scala3Version = "3.1.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "medeia-scala3-spike",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Vector(
      ("org.mongodb.scala" %% "mongo-scala-bson" % "4.4.0").cross(CrossVersion.for3Use2_13),
      "org.scalatest" %% "scalatest" % "3.2.10" % Test
    )
  )
