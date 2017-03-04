name := "cunning-linguist"

version := "1.0"

scalaVersion := "2.12.1"

// testing
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"

parallelExecution in Test := false

resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

