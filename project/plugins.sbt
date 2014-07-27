
resolvers ++= Seq(
  "devstack" at "https://devstack.io/repo/gravitydev/public"
)

libraryDependencies ++= Seq(
  "mysql" % "mysql-connector-java" % "5.1.31" % "compile",
  "com.gravitydev" %% "scoop" % "1.0.0-alpha11"
)
  
