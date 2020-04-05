name := "Oklahoma Covid-19 Tracker"
version := "0.0.1"

enablePlugins(ScalaJSPlugin)
scalaVersion := "2.12.8"                  // or any other Scala version >= 2.10.2
scalaJSUseMainModuleInitializer := true   // this is an application with a main method

resolvers += Resolver.bintrayRepo("hmil", "maven")

libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-dom" % "0.9.7",
  "fr.hmil" %%% "roshttp" % "2.2.4")

