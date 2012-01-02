organization := "m20o"

name := "scalatra-toys"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.9.1"

scalacOptions += "-unchecked"

seq(webSettings :_*)

libraryDependencies ++= Seq(
  "org.scalatra" %% "scalatra" % "2.0.2",
  "org.scalatra" %% "scalatra-scalate" % "2.0.2",
  "org.scalatra" %% "scalatra-specs2" % "2.0.2" % "test",
  "ch.qos.logback" % "logback-classic" % "1.0.0" % "runtime",
  "org.eclipse.jetty" % "jetty-webapp" % "7.4.5.v20110725" % "container",
  "javax.servlet" % "servlet-api" % "2.5" % "provided"
)

resolvers ++= Seq(
    "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
)

// Module dependencies
libraryDependencies ++= Seq(
  "junit" % "junit" % "4.7"
)

