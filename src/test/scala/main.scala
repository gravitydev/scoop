import org.scalatest.FunSuite
import com.gravitydev.scoop._, query._

class ScoopSuite extends FunSuite {
  import sample.Data._
  import sample.Models._
  import sample.Parsers

  Class forName "com.mysql.jdbc.Driver"
  implicit lazy val con = java.sql.DriverManager.getConnection("jdbc:mysql://localhost/gravitydev", "root", "")
  
  test ("Basic Functions") {
    // addition
    //val s: SelectExprS = (1: ast.SqlExpr[Int]) + 2
  }
  
  test ("query API") {
    val xx = decimal("SOMETHING", "SOMETHING2").columns

    val ids = using (tables.users as "u", tables.issues as "i") {(u,i) =>
      from(i)
        .innerJoin(u on i.reported_by === u.id)
        .limit(10)
        .find(i.id)
    }
    println("*"*50)
    println(ids)

    val res = using (tables.users as "hello") {u =>
      println("ALIAS")
      println(u.id as "somethingelse")
      val q = from(u)
        .where(u.email === "")
        .limit(10)
        .find(u.first_name)
    }

    val u = tables.users as "reporter"
    val i = tables.issues as "i"

    val nums = List(IssueStatuses.Open, IssueStatuses.Closed)
    val mapped = nums.map(v => i.status === v and i.status === v and i.status === v)
    val folded = mapped.foldLeft(false : ast.SqlExpr[Boolean])(_ or _)

    println(mapped)
    println(folded)

    {
      val x = "(" +~ from(u).where(u.id === 24) +~ ") UNION (" +~ from(i).where(i.id === 13) +~ ")"
    }

    {
      val q = from(u).where("u.id IN (" +~ from(u).where(u.id === 2) +~ ")")
    }    


    
    val userP = Parsers.user(u)
 
    val issueParser = i.id ~ i.status ~ userP ~ opt(userP) >> Issue.apply

    issueParser.columns map println

    val xparser = i.id ~ userP ~ long("test", sql="(SELECT 1)")

    val testParser = i.id ~ xparser

    val qq = from(i) select(testParser.columns:_*) 
    
    val num = "SELECT 1 as num FROM users WHERE 1 = ?" %? 1 //map int("num") head;

    val n = i.id |=| intToSqlLongLit(24)
 
    val q = from(i)
      .innerJoin (u on i.reported_by === u.id)
      //.where (u.first_name === "alvaro" and i.status === IssueStatuses.Open and i.status === 24)
      .where("reporter.last_name = ?" %? "carrasco")
      .orderBy (u.first_name desc, u.last_name asc)


     
    q find issueParser
    
    //val res = mapped(con)
    
    val j: Option[Long] = Some(4L)
    val v = i.assigned_to  := j

    //val v = i.assigned_to  := Some(4L)
    
    val x = insertInto(i).values(
      i.item_id     := 24,
      i.project_id  := 27,
      i.status      := IssueStatuses.Open
    )


    val assignee: Option[Long] = Option(1L)
  
    /*
    val y = update(i)
      .set(
        i.id          := 4L,
        i.assigned_to := assignee
      )
      .where(i.item_id === 24)

    */

    val vx = from(i)
      .select( (int("test", sql="1") >> {x => x}).columns:_* )

    println(vx.sql)
    
  }

  test ("utils") {
    
  implicit lazy val con = java.sql.DriverManager.getConnection("jdbc:mysql://localhost/gravitydev", "root", "")
    util.processQuery("SELECT 1 as first, 2 as second, 'something' as ha") {rs =>
      println(util.inspectRS(rs))
    }
  }
}

