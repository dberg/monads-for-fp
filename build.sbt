lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      name := "Monads for functional programming",
      organization := "com.cybergstudios",
      scalaVersion := "2.12.6",
      version      := "0.1.0-SNAPSHOT"
    )),
    scalacOptions := Seq(
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-unchecked",
      "-Xfatal-warnings",
      "-Xlint",
      "-Ywarn-dead-code",
      "-Ywarn-numeric-widen",
      "-Ywarn-value-discard",
      "-Ywarn-unused-import",
      "-Ypartial-unification"
    ),
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.0.5" % "test"
    )
  )

// Don't run tests when building the assembly jar.
test in assembly := {}
