// The Play plugin
addSbtPlugin("org.playframework" % "sbt-plugin" % "3.0.5")
addSbtPlugin("com.github.sbt" % "sbt-digest" % "2.0.0")

// Resolves similar issue to https://stackoverflow.com/questions/76693403/scala-play-dependency-issue
ThisBuild / libraryDependencySchemes += "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always

