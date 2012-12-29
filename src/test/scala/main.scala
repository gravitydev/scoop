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
    
    val num = "SELECT 1 as num FROM users WHERE 1 = ?".onParams(1) map int("num") head

    val n = i.id |=| intToSqlLongLit(24)
    println("VALUE")
    println(n)
 
    val q = from(i)
      .innerJoin (u on i.reported_by === u.id)
      //.where (u.first_name === "alvaro" and i.status === IssueStatuses.Open and i.status === 24)
      .where("reporter.last_name = ?" onParams "carrasco")
      .orderBy (u.first_name desc, u.last_name asc)
      
    //println(q)
      
    val test = q find issueParser
    
    //val res = mapped(con)
    
    //println(test)
    //println(res)
    val j: Option[Long] = Some(4L)
    val v = i.assigned_to  := j

    //val v = i.assigned_to  := Some(4L)
    
    val x = insertInto(i).set(
      i.item_id     := 24,
      i.project_id  := 27
    )

    val assignee: Option[Long] = Option(1L)
  
    /*
    val y = update(i)
      .set(
        i.id          := 4L,
        i.assigned_to := assignee
      )
      .where(i.item_id === 24)

    println(y.sql)
    println(y.params)
    
    println(x.sql)
    println(x.params)
    */
    
  }

  test ("utils") {
    util.processQuery("SELECT 1 as first, 2 as second, 'something' as ha") {rs =>
      println(util.inspectRS(rs))
    }
  }
}

