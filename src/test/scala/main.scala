import org.scalatest.{FlatSpec, Matchers}
import com.gravitydev.scoop._, query._

class ScoopSpec extends FlatSpec with Matchers {
  import sample.Data._
  import sample.Models._
  import sample.Parsers
  import ScoopMatchers._

  Class forName "com.mysql.jdbc.Driver"
  implicit lazy val con = java.sql.DriverManager.getConnection("jdbc:mysql://localhost/scoop_test", "root", "")
 
  "Basic number operators" should "work" in {
    val num: ast.SqlExpr[Int] = 1 

    (num + 2 as "x").selectSql

    (num + 2 as "a") should matchSelectSql("(? + ?) as a", 1, 2)
    (num - 2 as "a") should matchSelectSql("(? - ?) as a", 1, 2)
    (num * 2 as "a") should matchSelectSql("(? * ?) as a", 1, 2)
    (num / 2 as "a") should matchSelectSql("(? / ?) as a", 1, 2)
  }

  "Basic parsers" should "work" in {
    ("SELECT ? as num1, ? as num2" %? (5,6) process (sqlInt("num1") ~ sqlInt("num2"))).head should be ((5, 6))

    ("SELECT 1 as a, 2 as b, 3 as c, 4 as d, 5 as e" %? () process (
      sqlInt("a") ~ sqlInt("b") ~ sqlInt("c") ~ sqlInt("d") ~ sqlInt("e")
    )).head should be ((1, 2, 3, 4, 5))

    using (tables.users) {u =>
      (from(u).select(u.id, u.age) process (sqlInt(u.age.name))).head should be (30)
    }
  }

  "Selections" should "accumulate expressions" in {
    using (tables.users) {u =>
      val x = (u.age >> (_.toString)) ~ (u.first_name >> (_ + "x"))
    }
  }
  
  "Implicits" should "work" in {
    using (tables.issues as "i") {i =>
      (i.id := i.id + 1) should matchSql("id = (i.id + ?)", 1)
    }
  }
 
  "IS NOT NULL" should "work" in {
    using (tables.users) {u =>
      from(u)
        .where(u.email.isNotNull)
        .find(u.email)
        .list
    } should be (List("simplepic@gmail.com"))
  }

  "Parsers" should "work with aliased expressions" in {
    using (tables.users) {u =>
      val q = from(u)
        .select( functions.count( u.id ) as "x" )

      from(u)
        .find( Parsers.total( functions.count(u.id) ) as "x" )
        .list
    }
  }
  
  "Functions" should "output correct sql" in {
    import functions._

    coalesce(1, 0).as("total") should matchSelectSql("COALESCE(?, ?) as total", 1, 0)
    select(count(1).as("total"), 4 as "num") should matchSql("SELECT COUNT(?) as total, ? as num", 1, 4)
    select(coalesce(countDistinct(1), 0L) as "total", 4 as "num") should matchSql("SELECT COALESCE(COUNT(DISTINCT ?), ?) as total, ? as num", 1, 0, 4)

    using (tables.users as "u") {u =>
      (coalesce(u.id, 0L) as "v") should matchSelectSql("COALESCE(u.id, ?) as v", 0L)

      from(u).find(coalesce(countDistinct(u.id), 0L).as("total")).head should be (1)
    }
  }

  "Subquery" should "work in the SELECT clause" in {
    using (tables.users) {u =>
      from(u)
        .find(
          from(u)
            .limit(1)
            .select(u.id) as "a"
        )
        .list should be (List(1))
    }
  }

  "Subquery expression" should "work with EXISTS" in {
    using (tables.users) {u => 
      val q = from(u)
        .where(
          exists(from(u).select(u.id)) &&
          u.id === from(u).limit(1).select(u.id)
        )
        .select(u.id)

      /*
       * INSERT INTO cars (make, model)
       * SELECT 'ford', 'mustang'
       * FROM cars
       * ( WHERE NOT EXISTS
       *         (SELECT id FROM cars WHERE make = 'ford' AND model = 'mustang')
       * LIMIT 1;
       */

      /*
      using (tables.cars, tables.cars) {(c,inner) =>
        insertInto(c)(c.make, c.model).values(
          from(c)
            .where(
              notExists( 
                from(inner).where(inner.make === "ford" && inner.model === "mustang").select(inner.id) 
              )
            )
            .select("'ford', 'mustang'")
        )
      }
      */
    }
  }

  "Upserts" should "work" in {
    using (tables.users) {u =>
      val x = insertInto(u)
        .set(
          u.id := 1, 
          u.first_name := "Alvaro", 
          u.last_name := "Carrasco", 
          u.email := "simplepic@gmail.com",
          u.age := 31
        )
        .onDuplicateKeyUpdate(u.age := u.age + 2)()

      from(u).where(u.id === 1).find(u.age).head should be (32)

      update(u)
        .set(u.age := 30) 
        .where(u.id === 1)()
    }
  }

  "Ordering on expressions" should "work" in {
    using(tables.users) {u =>
      from(u)
        .where(u.id === 24L)
        .orderBy(u.id > 24L desc, u.id)
        .find(u.id)
        .list
    }
  }

  "Fragments" should "work anywhere on a query" in {
    using (tables.users) {u =>
      val ids: List[Long] = from("users u")
        .where("u.id = ?" %? 1)
        .find(sql[Long]("u.id" +~ "").as("id"))
        .list
    }
  }

  "A subquery" should "work on the FROM clause" in {
    using (tables.users) {u =>
      val sub = from(u).where(u.id === 1)

      val outer = from( sub as "s" )
    }
  }

  "Table and query aliases" should "be able to generate column aliases" in {
    using (tables.users) {u =>
      // sub query
      val sub = from(u).select(u.id) as "sub"

      // alias from table
      u[String]("first_name") should matchSql("users.first_name")

      // aliases from subquery
      sub[Long]("id") should matchSql("sub.id")

      sub(u.id) should matchSelectSql("sub.users_id as users_id")
    }
  }

  "A subquery" should "work on the JOIN clause" in {
    using (tables.users, tables.users) {(u,u2) =>
      // must define subquery first to obtain an alias
      val sub = from(u).where(u.id === 1).select(u.id) as "p"

      from(u2)
        .innerJoin(sub on u2.id === sub(u.id))
        .find(u2.id ~ sub(u.id))
        .list

      val q = from(u2)
        .innerJoin(sub on u2.id === sub(u.id))
        .select(u2.id, sub(u.id))
    }
  }

  "An update with Option[SqlAssignment]" should "inhibit None assignments" in {
    using (tables.users) {u =>
      val q = update(u)
        .set(
          Some(u.first_name := "check"),
          None,
          u.last_name := "Some other value"
        )
        .where(u.id === -1)

      q should matchSql(
        "UPDATE users SET first_name = ?, last_name = ? WHERE (users.id = ?)", 
        "check", 
        "Some other value", 
        -1
      )
    }
  }

  "A comparisons" should "work with literal types" in {
    using (tables.users) {u =>
      val p1: SqlExpr[Boolean] = u.first_name === "somename"
      val p2: SqlExpr[Boolean] = u.first_name like "somename"
      val p3: SqlExpr[Boolean] = u.first_name in Set("one", "two")
    }
  }

  "A string based subquery" should "work on the SELECT clause" in {
    using (tables.users) {u =>
      from(u).find( sql[Boolean]("(SELECT ?)" %? 1) as "someBool" ).list should be (List(true))
    }
  }

  "A string-based subquery" should "work on the FROM clause" in {
    using (tables.users) {u =>
      val sub = "" +~ from(u).sql +~ ""

      val outer = from( sub as "s" )
    }
  }

  "Parameterized sql on select" should "work" in {
    select("?" %? 1)
  }

  "Subquery on the from clause" should "work" in {
    using (tables.users) {u =>
      // must alias
      val subq = from(u).select(u.id, u.first_name) as "u"

      from( subq )
        .find( subq(u.id) )
        .list
    }
  }

  "Group By clause" should "work" in {
    using (tables.users) {u =>
      from(u)
        .groupBy(u.first_name, functions.coalesce(u.age, 0))
        .find(u.first_name ~ (functions.count(u.id) as "total"))
        .list
    }
  }

  "An assignment" should "allow expressions" in {
    using (tables.users) {u =>
      val q = update(u)
        .set(u.age := u.age + 1)

      q should matchSql("UPDATE users SET age = (users.age + ?)", 1)
    }
  }

  "Insert using select" should "work" in {
    using (tables.users) {u =>
      val insertQ = insertInto(u)(u.id, u.first_name).values(from(u).where(u.id === 24L).select(u.id, u.last_name))
    }
  }


  "Select single column from aliased tables" should "work" in {
    val ids = using (tables.users as "u", tables.issues as "i") {(u,i) =>
      from(i)
        .innerJoin(u on i.reported_by === u.id)
        .limit(10)
        .find(i.id)
        .list
    }
  }

  "Select from aliased tables" should "work" in {
    val res = using (tables.users as "hello") {u =>
      val q = from(u)
        .where(u.email === "")
        .limit(10)
        .find(u.first_name)
        .list
    }
  }

  "Random stuff" should "work" in {
    val s = select("'hello'")
    val xx = sql[scala.math.BigDecimal]("SOMETHING").as("SOMETHING2").expressions

    val u = tables.users as "reporter"
    val i = tables.issues as "i"

    val nums = List(IssueStatuses.Open, IssueStatuses.Closed)
    val mapped = nums.map(v => i.status === v and i.status === v and i.status === v)
    val folded = mapped.foldLeft(false : ast.SqlExpr[Boolean])(_ or _)

    {
      val x = "(" +~ from(u).where(u.id |=| 24) +~ ") UNION (" +~ from(i).where(i.id === 13) +~ ")"
    }

    {
      val q = from(u).where("u.id IN (" +~ from(u).where(u.id === 2) +~ ")")
    }     

    val userP = Parsers.user(u)

    val comb = userP ~ i.id
 
    val xparser = i.id ~ userP ~ sql[Long]("(SELECT 1)").as("test")

    val testParser = i.id ~ xparser
    val qq = from(i) select(testParser.expressions:_*) 
    val qx = from(i).where(i.id |=| subquery[Long](qq))

    val num = "SELECT 1 as num FROM users WHERE 1 = ?" %? 1 process sqlInt("num") head;

    val n = i.id |=| 24
 
  }

  "Explicit aliases" should "work" in {
    
    using (tables.issues as "i", tables.users as "reporter") {(i,u) =>
      tables.issues._tableName  should be ("issues")
      tables.issues.release_id.columnName should be ("release_id")
      i.release_id.columnName   should be ("release_id")
      i.release_id.name         should be ("i_release_id")

      from(i)
        .innerJoin (u on i.reported_by === u.id)
        .orderBy (u.first_name desc, u.last_name asc)
        .find(i.release_id)
        .list
    } 

  }

  "Query string" should "work in the WHERE clause" in {
    val i = tables.issues as "i"
    val u = tables.users as "reporter"

    val userP = Parsers.user(u)

    val issueParser = i.id ~ i.status ~ userP ~ opt(userP) ~ i.release_id >> Issue.apply

    val q = from(i)
      .innerJoin (u on i.reported_by === u.id)
      //.where (u.first_name === "alvaro" and i.status === IssueStatuses.Open and i.status === 24)
      .where("reporter.last_name = ?" %? "carrasco")
      .orderBy (u.first_name desc, u.last_name asc)
     
    q find issueParser
    

  }
  
  "Query API" should "work" in {
    val u = tables.users as "reporter"
    val i = tables.issues as "i"

    
    //val res = mapped(con)
    
    val j: Option[Long] = Some(4L)
    val v = i.assigned_to  := j

    //val v = i.assigned_to  := Some(4L)
    
    val x = insertInto(i).values(
      i.item_id     := 24,
      i.project_id  := 27,
      i.status      := IssueStatuses.Open
    )

    //println(x.sql)

    val z = insertInto(i).set(
      i.item_id     := 24,
      i.project_id  := 27,
      i.status      := IssueStatuses.Open
    )

    //println(z.sql)

    val assignee: Option[Long] = Option(1L)
  
    val y = update(i)
      .set(
        i.id          := 4L,
        i.assigned_to := assignee
      )
      .where(i.item_id === 24)

    //println(y.sql)


    val vx = from(i)
      .select( (sql[Int]("1").as("test") >> {x => x}).expressions:_* )
    
  }

}

