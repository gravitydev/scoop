import sbt._
import Keys._

object ScoopBuild extends Build {
  val gravityRepo = "gravitydev" at "https://devstack.io/repo/gravitydev/public"

  lazy val root = Project(id = "scoop", base = file(".")).settings(
    organization  := "com.gravitydev",
    name          := "scoop",
    version       := "1.0.0-alpha8-SNAPSHOT",
    crossScalaVersions := Seq("2.10.3", "2.9.3", "2.11.0-RC3"),
    publishTo := Some(gravityRepo),
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "1.9.2" % "test",
      "mysql"         % "mysql-connector-java"  % "5.1.18"  % "test"
    ),
    scalacOptions ++= Seq("-deprecation","-unchecked"/*,"-XX:-OmitStackTraceInFastThrow"*/),
    testOptions in Test += Tests.Argument("-oF"),
    sourceGenerators in Compile += {
      scoopGenerateTask.task
    },
    scoopJdbcUrl := "jdbcurlstuff",
    scoopGenerateTask := {
      println(scoopJdbcUrl.value)
      Seq[java.io.File]()
    }
  )

  lazy val plugin = Project(id = "scoop-sbt-plugin", base = file("sbt-plugin")).settings(
    sbtPlugin := true,
    libraryDependencies := Seq(
      "mysql" % "mysql-connector-java" % "5.1.18" % "compile"
    )	  
  )

  val scoopJdbcUrl = settingKey[String]("The JDBC URL for the database to inspect.")
  val scoopJdbcUsername = settingKey[String]("The JDBC username for the the database to inspect.")
  val scoopJdbcPassword = settingKey[String]("The JDBC password for the database to inspect.")

  val scoopGenerateTask = taskKey[Seq[java.io.File]]("Generate the Scoop metadata for the configured database.")

}

