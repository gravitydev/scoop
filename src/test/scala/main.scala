import org.scalatest.FunSuite

import com.gravitydev.scoop._, query._

class ScoopSuite extends FunSuite {
  import Repo._
  
  Class forName "com.mysql.jdbc.Driver"
  implicit val con = java.sql.DriverManager.getConnection("jdbc:mysql://localhost/gravitydev", "root", "")
  
  test ("query API") {
    case class User (first: String, last: String)
    case class Issue (id: Long, assignee: User)
    
    def userParser (u: users) = u.first_name ~ u.last_name >> User.apply
    
    val res = using (users as "hello") {u =>
      println(u)
      println(u.first_name)
      val q = from(u)
        .where(u.email === "")
        .select(u.first_name)
      println(q)
      ""
    }
    
    val u = users as "reporter"
    val i = issues as "i"
    
    val userP = userParser(u)
    val issueParser = i.id ~ userP >> Issue.apply

    val xparser = i.id ~ userP ~ long("test", sql="(SELECT 1)")

    val testParser = i.id ~ xparser

    val qq = from(i) select(testParser.columns:_*) 

    println(qq)
    
    println("USER COLS")
    println(issueParser.columns.map(_.sql))
    println("DONE")
 
    val q = from(i)
      .innerJoin (u on i.reported_by === u.id)
      .where (u.first_name === "alvaro")
      .where("reporter.last_name = ?" onParams "carrasco")
      .orderBy (u.first_name desc, u.last_name asc)
      .select (issueParser.columns:_*)
      
    //println(q)
      
    val test = q map issueParser
    
    //val res = mapped(con)
    
    //println(test)
    //println(res)
    
    val x = insertInto(i).set(
      i.item_id     := 24,
      i.project_id  := 27
    )
    
    println(x.sql)
    println(x.params)
    
  }

  test ("utils") {
    util.processQuery("SELECT 1 as first, 2 as second, 'something' as ha") {rs =>
      println(util.inspectRS(rs))
    }
  }
}

