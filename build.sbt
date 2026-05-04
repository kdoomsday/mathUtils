name := "mathUtils"

organization := "com.ebarrientos"

version := "1.1"

scalaVersion := "3.8.3"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.16.0" % "test"
