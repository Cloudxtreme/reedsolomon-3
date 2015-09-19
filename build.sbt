name := "Reed-Solomon ECC"

version := "0.1"

organization := "edu.jingw"

scalaVersion := "2.11.7"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")

resolvers ++= Seq(
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.12.5" % "test",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test"
)

org.scalastyle.sbt.ScalastylePlugin.Settings

coverageEnabled := true
