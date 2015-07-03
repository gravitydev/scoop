import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import com.gravitydev.scoop._, query._

class ScoopSpec extends FlatSpec with ShouldMatchers {
  import sample.Data._
  import sample.Models._
  import sample.Parsers
  import ScoopMatchers._

  Class forName "com.mysql.jdbc.Driver"
  implicit lazy val con = java.sql.DriverManager.getConnection("jdbc:mysql://localhost/scoop_test", "root", "")

  "Basic number operators" should "work" in {
    val num: ast.SqlExpr[Int] = 1 

    (num + 2 as "a") should matchSelectSql("(? + ?) as a", 1, 2)
    (num - 2 as "a") should matchSelectSql("(? - ?) as a", 1, 2)
    (num * 2 as "a") should matchSelectSql("(? * ?) as a", 1, 2)
    (num / 2 as "a") should matchSelectSql("(? / ?) as a", 1, 2)
  }

  "Literal expressions" should "work" in {
    ("SELECT 1" %? () process literal(24L)).head should be (24L)

    using (tables.users) {u =>
      from(u)
        .find(u.id ~ literal("test")).head should be ((1L, "test"))
    }
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
      (i.id := i.id + 1) should matchSql("id = i.id + ?", 1)
    }
  }
 
  "IS NOT NULL" should "work" in {
    using (tables.users) {u =>
      deleteFrom(u)
        .where(u.email === "some@email.com")()

      insertInto(u)
        .set(
          u.first_name := "James",
          u.last_name := "X",
          u.email := "some@email.com",
          u.age := 20,
          u.nickname := None
        )()

      update(u)
        .set(u.nickname := Some("dude"))
        .where(u.id === 1)()

      val res = from(u)
        .where(u.nickname.isNotNull)
        .find(u.email)
        .list

      update(u)
        .set(u.nickname := None)
        .where(u.id === 1)()

      deleteFrom(u)
        .where(u.email === "some@email.com")()
    
      res

    } should be (List("simplepic@gmail.com"))
  }

  "Parsers" should "work with aliased expressions" in {
    using (tables.users) {u =>
      val q = from(u)
        .select( functions.count( u.id ) as "x" )

      from(u)
        .find( functions.count(u.id) as "x" )
        .list
    }
  }

  "Functions" should "output correct sql" in {
    import functions._

    coalesce(1, 0).as("total") should matchSelectSql("COALESCE(?, ?) as total", 1, 0)

    select(
      count(1).as("total"), 
      4 as "num"
    ) should matchSql("SELECT COUNT(?) as total, ? as num", 1, 4)

    select(
      coalesce(countDistinct(1), 0L) as "total", 
      4 as "num"
    ) should matchSql("SELECT COALESCE(COUNT(DISTINCT ?), ?) as total, ? as num", 1, 0, 4)

    using (tables.users as "u") {u =>
      (coalesce(u.id, 0L) as "v") should matchSelectSql("COALESCE(u.id, ?) as v", 0L)

      from(u)
        .where(u.email === "simplepic@gmail.com")
        .find(
          coalesce(countDistinct(u.id), 0L) as "total"
        )
        .head should be (1)
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

      from(u)
        .where(u.id === 1)
        .find(u.age)
        .head should be (32)

      update(u)
        .set(u.age := 30) 
        .where(u.id === 1)()
    }
  }

  "Ordering" should "work with expressions" in {
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
      val ids: List[Long] = from(u as "u")
        .where(sqlExpr[Boolean]("u.id = ?" %? 1))
        .find(sqlExpr[Long]("u.id" +~ "").as("id"))
        .list
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
        "UPDATE users SET first_name = ?, last_name = ? WHERE users.id = ?", 
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

  "A like comparison" should "work with expressions with String as their underlying type" in {
    using (tables.users) {u =>
      // native string
      u.first_name like "somename"

      // custom (underlying string)
      u.role like "somerole"
    }
  }

  "Parameterized sql on select" should "work" in {
    select(sqlExpr[Int]("?" %? 1) as "a")
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

      q should matchSql("UPDATE users SET age = users.age + ?", 1)
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
    val s = select(sqlExpr[String]("'hello'").as("a"))
    val xx = sqlExpr[scala.math.BigDecimal]("SOMETHING").as("SOMETHING2").expressions

    val u = tables.users as "reporter"
    val i = tables.issues as "i"

    val nums = List(IssueStatuses.Open, IssueStatuses.Closed)
    val mapped = nums.map(v => i.status === v and i.status === v and i.status === v)
    val folded = mapped.foldLeft(false : ast.SqlExpr[Boolean])(_ or _)

    {
      val x = 
        "(" +~ from(u).where(u.id |=| 24).select(u.id) +~ ") UNION (" +~ from(i).where(i.id === 13).select(u.id) +~ ")"
    }

    {
      val q = 
        from(u)
          .where(
            sqlExpr[Boolean]("u.id IN (" +~ from(u).where(u.id === 2).select(u.id) +~ ")")
          )
    }     

    val userP = Parsers.user(u)

    val comb = userP ~ i.id
 
    val xparser = i.id ~ userP ~ sqlExpr[Long]("(SELECT 1)").as("test")

    val testParser = i.id ~ xparser
    val qq = from(i) select(i.id) 
    val qx = from(i).where(i.id |=| qq)

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
      .where(
        sqlExpr[Boolean]("reporter.last_name = ?" %? "carrasco")
      )
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

    val z = insertInto(i).set(
      i.item_id     := 24,
      i.project_id  := 27,
      i.status      := IssueStatuses.Open
    )

    val assignee: Option[Long] = Option(1L)
  
    val y = update(i)
      .set(
        i.id          := 4L,
        i.assigned_to := assignee
      )
      .where(i.item_id === 24)

    val vx = from(i)
      .select( 
        (sqlExpr[Int]("1").as("test") >> {x => x})
      )
    
  }

  "A somewhat complex query" should "work" in {
    val query = using (tables.users, tables.users, tables.issues) {(u,u2,i) => 
      from(u)      
        .innerJoin(u2 on u2.age === u.id)
        .leftJoin(i on i.assigned_to === u.id)
        .where(
          u.age < 24 && 
          (u.id === 27 || u.id === 23 || u.id === 7) &&
          (u.id |=| from(u).select(29L as "num"))
        )
        .limit(100)
        .offset(10)
        .orderBy(i.id, i.assigned_to.desc, 24.desc)
        .select(u.id, u.first_name, u.last_name, u2.age)
    }
    println(query)
    val statement: ParameterizedSql = query
    println(statement.sql)
    println(statement.params)
  }

}

