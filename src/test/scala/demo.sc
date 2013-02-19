import com.gravitydev.scoop._, query._
import java.sql.Connection
import sample.Data._

object demo {
  implicit val con = java.sql.DriverManager.getConnection("jdbc:mysql://localhost/gravitydev", "root", "")
                                                  //> con  : java.sql.Connection = com.mysql.jdbc.JDBC4Connection@6766afb3
  
  "hello" + "test"
  
  /*
  tables.users

  val x = using(tables.users)(u => ())
  
  println(x)
  */

}