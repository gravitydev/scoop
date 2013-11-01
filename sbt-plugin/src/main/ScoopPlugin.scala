import sbt._

object ScoopPlugin extends Plugin {
  val generate = TaskKey[Unit]("scoop-generate", "Generate scala code to interact with JDBC database.")

  val generateTask = generate <<= {
    println("scoop generate")
  }
}

