import sbt._
import Keys._

object ScoopBuild extends Build {
  val gravityRepo = "gravitydev" at "http://repos.gravitydev.com/app/repos/12"

  lazy val root = Project(id = "scoop", base = file(".")).settings(
    organization  := "com.gravitydev",
    name          := "scoop",
    version       := "0.1.7-SNAPSHOT",
    crossScalaVersions := Seq("2.9.1", "2.9.2", "2.10.0"),
    publishTo := Some(gravityRepo),
    libraryDependencies ++= Seq(
      "org.scalatest" %%  "scalatest"             % "1.8"     % "test" cross CrossVersion.full,
      "mysql"         %   "mysql-connector-java"  % "5.1.18"  % "test"
    ),
    scalacOptions ++= Seq("-deprecation","-unchecked"/*,"-XX:-OmitStackTraceInFastThrow"*/),
    testOptions in Test += Tests.Argument("-oF")
  )

  /*
  lazy val plugin = Project(id = "scoop-sbt-plugin", base = file("sbt-plugin")).settings(
    sbtPlugin := true,
    libraryDependencies := Seq(
      "mysql" % "mysql-connector-java" % "5.1.18" % "compile"
    )	  
  )
  */
}

