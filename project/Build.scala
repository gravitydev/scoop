import sbt._
import Keys._

object ScoopBuild extends Build {
  val gravityRepo = "gravitydev" at "https://devstack.io/repo/gravitydev/public"

  lazy val root = Project(id = "scoop", base = file(".")).settings(
    organization  := "com.gravitydev",
    name          := "scoop",
    version       := "0.2.3-SNAPSHOT",
    crossScalaVersions := Seq("2.10.3", "2.9.2"),
    publishTo := Some(gravityRepo),
    libraryDependencies ++= Seq(
      "org.scalatest" % "scalatest_2.10" % "2.0" % "test",
      "mysql"         % "mysql-connector-java"  % "5.1.18"  % "test"
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

