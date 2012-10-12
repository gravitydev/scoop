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
    
    val u = users as "u"
    val i = issues as "i"
    
    val y: ast.SqlExpr[Long] = i.id
    
    //val j = issues.assigned_to === 1L
    
    val res = from(u).where(u.id in Set(1L, 2L, 3L))
    
    
    println(res)
 
    val test = from(u)
      .innerJoin (i on i.id === u.id)
      .where (i.id === 1L or i.id >= i.id and i.id.isNull and (i.title like "%24"))
      .orderBy (u.first_name desc, u.last_name asc)
      .select (u.first_name, u.last_name)
    
    //val res = mapped(con)
    
    println(test)
    //println(res)
  }
}
