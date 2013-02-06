import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers._
import com.gravitydev.scoop._, query._

class ScoopSpec extends FlatSpec {
  import sample.Data._
  import sample.Models._
  import sample.Parsers
  import ScoopMatchers._

  Class forName "com.mysql.jdbc.Driver"
  implicit lazy val con = java.sql.DriverManager.getConnection("jdbc:mysql://localhost/gravitydev", "root", "")
  
  "Basic Functions" should "work" in {
    // addition
    {
      val x: SelectExprS = (1: ast.SqlExpr[Int]) + 2
      x should matchSql("(? + ?)", 1, 2)
    }
    {
      val x: SelectExprS = (1: ast.SqlExpr[Int]) - 2
      x should matchSql("(? - ?)", 1, 2)
    }
  }
  
  "Implicits" should "work" in {
    using (tables.issues as "i") {i =>
      i.id + 1 as "test"
    }
  }
  
  "Query API" should "work" in {
    using (tables.users) {u =>
      val insertQ = insertInto(u)(u.id, u.first_name).values(from(u).where(u.id === 24L).select(u.id, u.last_name))
      println(insertQ.sql)
      println(insertQ.params)
    }
    
    val xx = decimal("SOMETHING", "SOMETHING2").columns

    val ids = using (tables.users as "u", tables.issues as "i") {(u,i) =>
      from(i)
        .innerJoin(u on i.reported_by === u.id)
        .limit(10)
        .find(i.id)
    }

    val res = using (tables.users as "hello") {u =>
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

    {
      val x = "(" +~ from(u).where(u.id === 24) +~ ") UNION (" +~ from(i).where(i.id === 13) +~ ")"
    }

    {
      val q = from(u).where("u.id IN (" +~ from(u).where(u.id === 2) +~ ")")
    }    


    
    val userP = Parsers.user(u)
 
    val issueParser = i.id ~ i.status ~ userP ~ opt(userP) >> Issue.apply

    println(issueParser.getClass)
    println(issueParser)

    issueParser.columns map println

    val xparser = i.id ~ userP ~ long("test", sql="(SELECT 1)")

    val testParser = i.id ~ xparser

    val qq = from(i) select(testParser.columns:_*) 
    
    val num = "SELECT 1 as num FROM users WHERE 1 = ?" %? 1 map int("num") head;

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
    
  }

  "utils" should "work" in {
    util.processQuery("SELECT 1 as first, 2 as second, 'something' as ha") {rs =>
      //println(util.inspectRS(rs))
    }
  }
}

