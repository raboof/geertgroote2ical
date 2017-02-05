resolvers += Resolver.sonatypeRepo("public")

scalacOptions := Seq("-feature", "-deprecation")

scalaVersion := "2.12.1"

//scalafmtConfig in ThisBuild := Some(file(".scalafmt"))

fork in run := true

libraryDependencies += "net.bzzt" %% "scala-icalendar" % "0.0.1-SNAPSHOT"
libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.7.22"
libraryDependencies += "net.bzzt" %% "ectrace" % "0.0.2-SNAPSHOT"
libraryDependencies += "net.ruippeixotog" %% "scala-scraper" % "1.2.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"

libraryDependencies += "com.amazonaws" % "aws-lambda-java-core" % "1.0.0"
libraryDependencies += "net.databinder.dispatch" %% "dispatch-core" % "0.12.0"
