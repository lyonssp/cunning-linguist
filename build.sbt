name := "cunning-linguist"

version := "1.0"

scalaVersion := "2.12.1"

// Last stable release
libraryDependencies += "org.scalanlp" %% "breeze" % "0.13"

// Native libraries are not included by default. add this if you want them (as of 0.7)
// Native libraries greatly improve performance, but increase jar sizes. 
// It also packages various blas implementations, which have licenses that may or may not
// be compatible with the Apache License. No GPL code, as best I know.
libraryDependencies += "org.scalanlp" %% "breeze-natives" % "0.13"

// The visualization library is distributed separately as well.
// It depends on LGPL code
libraryDependencies += "org.scalanlp" %% "breeze-viz" % "0.13"

libraryDependencies += "org.typelevel" %% "cats" % "0.9.0"

// testing
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"

parallelExecution in Test := false

resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"
resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
