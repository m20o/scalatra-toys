Introduction
------------

**Scalatra-toys** contains a small set of extensions for the excellent [Scalatra](http://www.scalatra.org) web framework.
For detailed usage instructions please see the [wiki](https://github.com/m20o/scalatra-toys/wiki).

Requirements
------------

* [Scalatra](http://www.scalatra.org) version 2.0.x (developed and tested with version 2.0.2)
* [Scala](http://www.scala-lang.org) version 2.9.1


Installation
------------

Currently there are no released versions of the project but *snapshots* can be taken directly from GitHub in the following
manners:

**sbt 0.11.x** : add the following lines to PROJECT_DIR/build.sbt

```scala
   resolvers += "scalatra-toys-repo" at "http://m20o.github.com/scalatra-toys/m2/"

   libraryDependencies += "com.github.m20o" %% "scalatra-toys" % "0.1.1-SNAPSHOT"
```


