[![Build Status](https://travis-ci.org/gravitydev/scoop.svg?branch=rs-iterator2)](https://travis-ci.org/gravitydev/scoop)

Scoop
=====

Scoop is a query construction toolkit for scala. It is a very early experiment. You could say it is a strange mix between Squeryl and Anorm.
The main motivation is to provide a concise and practical solution that doesn't necessarily take complete control away from the SQL strings used. 

Lots of inspiration was taken from squeryl, anorm, and the apocalisp blog.

Installation
------------

SBT:
```sbt
resolvers += "gravity" at "https://devstack.io/repo/gravitydev/public"

libraryDependencies += "com.gravitydev" %% "scoop-0.2.0-SNAPSHOT"
```

Model Definition
----------------

Since the main purpose is to construct queries, the model definition should pretty much mirror the database.

```scala
import com.gravitydev.scoop._

class users extends Table[users](users) {
  val id          = col[Long]           ('id)
  val first_name  = col[String]         ('first_name)
  val last_name   = col[String]         ('last_name)
  val age         = col[Int]            ('age)
  val nickname    = col[String]         ('nickname)    nullable
}
```

Query API 
---------

It looks a lot like SQL and you can 
actually combine the model objects with custom query strings.

```scala
import com.gravitydev.scoop._, query._

val query = using(issues, users as "reporter", users as "assignee") {(i, r, a) =>
  from(i)
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
}
```

Mapping and Parsers
-------------------

Parsers allow you to create a composable mapping from a ResultSet to your own object. Aliases are handled nicely. 
This is important if your query might be joining the same table twice or is using a different alias for a particular table.

```scala
case class Account (
  id: Long,
  name: String
)

case class User (
  id: Long,
  name: String,
  account: Option[Account]
)

object Parsers {
  // make sure the case class has the correct number and type of parameters
  def account (a: accounts) = a.id ~ a.name >> Account.apply

  // you can use an existing parser when defining a new one:
  def user (u: users, a: accounts) = u.id ~ u.first_name ~ u.last_name ~ opt(account(a)) >> User.apply
}

// instantiate a parser by specifying the tables it should use
// the tables can be configured with aliases
// this makes the definition of the parser general, but the instantiation custom to the query
val users = from(users)
  .leftJoin(accounts on employees.manager_id === managers.id)
  .find(Parsers.user(users, accounts))

```

