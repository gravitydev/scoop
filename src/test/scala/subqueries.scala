import org.scalatest.{FlatSpec, Matchers}
import com.gravitydev.scoop._, query._

class SubqueriesSpec extends FlatSpec with Matchers {
  import sample.Data._
  import sample.Models._
  import sample.Parsers
  import ScoopMatchers._

  Class forName "com.mysql.jdbc.Driver"
  implicit lazy val con = java.sql.DriverManager.getConnection("jdbc:mysql://localhost/scoop_test", "root", "")
 
  "Subquery" should "work in the SELECT clause" in {
    using (tables.users) {u =>
      val sub = from(u).select(u.age as "age")
    
      sub as "a"

      println(sub)

      from(u)
        .find(
          from(u)
            .limit(1)
            .select(u.age) as "a"
        )
        .list should be (List(30))
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

  "A subquery" should "work on the FROM clause" in {
    using (tables.users) {u =>
      val sub = from(u).where(u.id === 1)

      sub as "s"

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

  "Subquery on the from clause" should "work" in {
    using (tables.users) {u =>
      // must alias
      val subq = from(u).select(u.id, u.first_name) as "u"

      from( subq )
        .find( subq(u.id) )
        .list
    }
  }  

}

