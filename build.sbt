organization := "com.github.m20o"

name := "scalatra-toys"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.9.1"

scalacOptions ++= Seq("-deprecation", "-unchecked")

libraryDependencies ++= Seq(
  "org.scalatra" %% "scalatra" % "2.0.2",
  "org.scalatra" %% "scalatra-scalate" % "2.0.2",
  "org.scalatra" %% "scalatra-specs2" % "2.0.2" % "test",
  "ch.qos.logback" % "logback-classic" % "1.0.0" % "runtime",
  "javax.servlet" % "servlet-api" % "2.5" % "provided"
)

resolvers ++= Seq(
    "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
)

publishTo := Some(Resolver.file("Github Pages", Path.userHome / "github" / "m20o.github.com" / "m2" asFile)(Patterns(true, Resolver.mavenStyleBasePattern)))

publishMavenStyle := true