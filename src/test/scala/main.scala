import org.scalatest.FunSuite

import com.gravitydev.scoop._, collection._, strong._, query._

class ScoopSuite extends FunSuite {
  import Repo._
  
  Class forName "com.mysql.jdbc.Driver"
  implicit val con = java.sql.DriverManager.getConnection("jdbc:mysql://localhost/gravitydev", "root", "")
  
  test ("strong API") {
    import strong._;
    /*
    val q = from (users, users)((r, a) =>
      select(r.first_name, a.last_name, a.id)
    ) map {case x :+: y :+: age :+: HNil =>
      println(x)
    }*/
    
    /*
    val q = from(users)(u => 
      select(u.first_name, u.last_name, u.id)
    )
    
    case class User(first: String, last: String, age: Long)
    
    val mapped = q map {case f :+: l :+: i :+: HNil =>
      User(f,l,i)
    }
    */
  }
  
  test ("query API") {
    import query._

    case class User (first: String, last: String)
    case class Issue (id: Long, assignee: User)
    
    def userParser (u: Users) = u.first_name ~ u.last_name >> User.apply
    
    val u = users as "u" prefix "reporter_"
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
      .addWhere("u.last_name = ?", "carrasco")
      .orderBy (u.first_name desc, u.last_name asc)
      .select (issueParser.columns:_*)
      
    println(q)
      
    val test = q map issueParser
    
    //val res = mapped(con)
    
    println(test)
    //println(res)
    
    val x = insertInto(i).set(
      i.item_id     := 24,
      i.project_id  := 27
    )
    
  }
}
