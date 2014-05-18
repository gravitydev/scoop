import sbt._
import Keys._

object ScoopBuild extends Build {
  val gravityRepo = "gravitydev" at "https://devstack.io/repo/gravitydev/public"

  lazy val root = Project(id = "scoop", base = file(".")).settings(
    organization  := "com.gravitydev",
    name          := "scoop",
    version       := "1.0.0-alpha9",
    scalaVersion  := "2.11.0",
    crossScalaVersions := Seq("2.11.0", "2.10.4"),
    publishTo := Some(gravityRepo),
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "2.1.6" % "test",
      "mysql"         % "mysql-connector-java"  % "5.1.30"  % "test"
    ),
    scalacOptions ++= Seq("-deprecation","-unchecked"/*,"-XX:-OmitStackTraceInFastThrow"*/),
    testOptions in Test += Tests.Argument("-oF")
  )

  /*
  lazy val plugin = Project(id = "scoop-sbt-plugin", base = file("sbt-plugin")).settings(
    sbtPlugin := true,
    libraryDependencies := Seq(
      "mysql" % "mysql-connector-java" % "5.1.30" % "compile"
    )	  
  )
  */
}

