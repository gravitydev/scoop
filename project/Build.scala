import sbt._
import Keys._

object ScoopBuild extends Build {
  val gravityRepo = "gravitydev" at "https://devstack.io/repo/gravitydev/public"

  lazy val root = Project(id = "scoop", base = file("."))
    .settings(
      organization  := "com.gravitydev",
      name          := "scoop",
      version       := "1.1.0-RC1",
      scalaVersion  := "2.11.6",
      crossScalaVersions := Seq("2.11.1", "2.10.4"),
      publishTo := Some(gravityRepo),
      libraryDependencies ++= Seq(
        "com.googlecode.kiama" %% "kiama" % "2.0.0-SNAPSHOT",
        "org.scalatest" %% "scalatest" % "2.1.6" % "test",
        "mysql"         % "mysql-connector-java"  % "5.1.35"  % "test",
        "com.github.mpilquist" %% "simulacrum" % "0.3.0"
      ),
      resolvers ++= Seq(
        "nexus" at "https://oss.sonatype.org/content/repositories/snapshots"
      ),
      scalacOptions ++= Seq("-deprecation","-unchecked"),
      testOptions in Test += Tests.Argument("-oF"),
      addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)
    )

  lazy val plugin = Project(id = "scoop-sbt-plugin", base = file("sbt-plugin"))
    .settings(
      sbtPlugin := true,
      organization := "com.gravitydev",
      version := "0.0.3-SNAPSHOT",
      publishTo := Some(gravityRepo),
      libraryDependencies := Seq(
        "com.gravitydev" %% "scoop" % "1.0.0-alpha12",
        // TODO: support other dbs
        "mysql" % "mysql-connector-java" % "5.1.35" % "compile"
      )	  
    )
}

