name := "Reed-Solomon ECC"

version := "0.1"

organization := "edu.jingw"

scalaVersion := "2.10.3"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")

resolvers ++= Seq(
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.11.0" % "test",
  "com.novocode" % "junit-interface" % "0.8" % "test->default",
  "junit" % "junit" % "4.11" % "test"
)

org.scalastyle.sbt.ScalastylePlugin.Settings
