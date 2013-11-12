name := "Reed-Solomon ECC"

version := "0.1"

organization := "edu.jingw"

scalaVersion := "2.10.3"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")

resolvers ++= Seq(
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.10.1" % "test",
  "org.scalatest" % "scalatest_2.10" % "2.0" % "test"
)

org.scalastyle.sbt.ScalastylePlugin.Settings
