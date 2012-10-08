Scoop
=====

Scoop is a query construction toolkit for scala. It is a very early experiment. You could say it is a strange mix between Squeryl and Anorm.
The main motivation is to provide a concise and practical solution that doesn't necessarily take complete control away from the SQL strings used. 

Installation
------------

TODO: Maven Repository

Model Definition
----------------

Since the main purpose is to construct queries, the model definition should pretty much mirror the database.

```scala
import com.gravitydev.scoop._

class Users (as: String) extends Table[Users]("users", Users) {
  val id          = col[Long]           ("id")
  val first_name  = col[String]         ("first_name")
  val last_name   = col[String]         ("last_name")
  val age         = col[Int]            ("age")
  val nickname    = col[Option[String]] ("nickname")
}
def users = Users("u") // useful for creating default alias
```

Strong API
----------

*Not quite ready yet* The strong API is mostly modeled after squeryl (though nowhere near as featured at the moment).
It tries to provide the stronges guarantees that if your code compiles, the generated SQL is free of syntax (and some semantic) errors. 
Since you don't define the data model using classes representing rows, you have to do your own mapping though. In that sense, it is closer to ScalaQuery than Squeryl.
The syntax is also just a bit closer to SQL since you can use symbols like >, <=, etc even with primitives.

```scala
import com.gravitydev.scoop._, strong._

val query = from(users)(u => 
  where(u.age > 24 and u.name === "alvaro")
  select(u.first_name, u.last_name)
)
```

Query API (stringly typed)
--------------------------

*Somewhat usable* This API sacrifices some safety for flexibility and in some cases readability. It looks a bit more like SQL and you can 
actually combine the model objects with custom query strings.

The main differences compared with SQL:
 * You should define the aliases before hand so they are available throughout the query.
 * *select* is done last.

```scala
import com.gravitydev.scoop._, query._

val i = issues
val r = users as "reporter"
val a = users as "assignee"

val query = from(i)
  .innerJoin(r on i.reporter_id === r.id)
  .leftJoin(a on i.assigned_to === a.id)
  .where(r.accountId isNotNull)
  .orderBy(i.status desc, i.reason desc)
  .select(
    i.*, 
    r.first_name as "reporter", 
    a.first_name as "assignee", 
    "(SELECT COUNT(*) FROM stats WHERE stats.issue_id = " + i.id.sql + ") as total_stats"
  )
```

Mapping and Parsers
-------------------

```scala
case class Account (
  id: Long,
  name: String
)
object Account {
  def parser (a: Account) = (a.id ~ a.name) map {case (i,n) => Account(i,n)}
}

case class User (
  id: Long,
  name: String,
  account: Option[Account]
)
object User {
  def parser (u: Users, a: Accounts) = (u.id ~ u.first_name ~ u.last_name ~ opt(Accounts.parser(a)) map {
    case id~first~last~manager => User(id, first+" "+last, manager)
  }
}

val query = from(users)
  .leftJoin(accounts on employees.manager_id === managers.id)
  .map {User.parser _}

val res: Seq[User] = query(con) 
```

Acknowledgements
----------------

Lots of inspiration was taken from squeryl, anorm, and the apocalisp blog.

