import sbt._
import Keys._

object ScoopBuild extends Build {
  lazy val root = Project(id = "scoop", base = file(".")).settings(
    organization  := "com.gravitydev",
    name          := "scoop",
    version       := "0.0.1-SNAPSHOT",
    crossScalaVersions := Seq("2.8.1", "2.9.1"),
    libraryDependencies ++= Seq(
      "org.scalatest" %%  "scalatest"             % "2.0.M4"     % "test",
      "mysql"         %   "mysql-connector-java"  % "5.1.18"  % "test"
    )	  
  )

  /*
  lazy val plugin = Project(id = "reap-sbt-plugin", base = file("sbt-plugin")).settings(
    sbtPlugin := true,
    libraryDependencies := Seq(
      "mysql" % "mysql-connector-java" % "5.1.18" % "compile"
    )	  
  )
  */
}

