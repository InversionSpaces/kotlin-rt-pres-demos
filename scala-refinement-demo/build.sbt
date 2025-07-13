val scala3Version = "3.7.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Scala Refined Demo",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "eu.timepit" %% "refined" % "0.11.3"
    ),
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test
  )
