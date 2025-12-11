val scala3Version = "3.8.0-RC3"

scalaVersion := scala3Version

val sharedSettings = Seq(
  scalaVersion := scala3Version,
  scalacOptions ++= Seq(
    "-new-syntax",
    // "-no-indent",
    "-Wvalue-discard",
    "-Wunused:all",
    // "-Werror",
    "-deprecation",
    "-explain",
    "-explain-cyclic",
    "-rewrite",
    "-source:future"
  ),
  javacOptions ++= Seq("-source", "25", "-target", "25")
)

lazy val decoda =
  // select supported platforms
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .crossType(CrossType.Full) // [Pure, Full, Dummy], default: CrossType.Full
    .in(file("decoda"))
    .settings(sharedSettings *)
    .settings(
      name := "decoda",
      organization := "io.decoda",
      version := "0.0.1"
    )
    .jsSettings( /* ... */ ) // defined in sbt-scalajs-crossproject
    .jvmSettings(
      libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"
    )
    // configure Scala-Native settings
    .nativeSettings( /* ... */ ) // defined in sbt-scala-native
