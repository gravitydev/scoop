Overview
========

Scoop is a query construction toolkit for scala. You could say it is a strange mix between Squeryl and Anorm.
The main motivation is to provide a concise and practical solution that doesn't necessarily take complete control away from the SQL strings used. 

Lots of inspiration was taken from squeryl, anorm, and the apocalisp blog.

Features:
* Query DSL that looks very similar to SQL (type-safe to an extent)
* Composable ResultSet parsing
* Easy to mix raw SQL when needed

Installation
------------

TODO: Maven Repository

Model Definition
----------------

Since the main purpose is to construct queries, the model definition should pretty much mirror the database.

.. code-block:: scala

  import com.gravitydev.scoop._

  class users extends Table[users](users) {
    val id          = col[Long]           ("id")
    val first_name  = col[String]         ("first_name")
    val last_name   = col[String]         ("last_name")
    val age         = col[Int]            ("age")
    val nickname    = col[String]         ("nickname")    nullable
  }

Learn more: :ref:`data-model`

Query API 
---------

This API sacrifices some safety for flexibility and in some cases readability. It looks a bit more like SQL and you can 
actually combine the model objects with custom query strings.

The main differences compared with SQL:
 * You should define the aliases before hand so they are available throughout the query.
 * *select* is done last.

.. code-block:: scala

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

Learn more: :ref:`usage`

Mapping and Parsers
-------------------

Parsers allow you to create a composable mapping from a ResultSet to your own object. Aliases are handled nicely. 
This is important if your query might be joining the same table twice or is using a different alias for a particular table.

.. code-block:: scala

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
  val userParser = Parsers.user( users, accounts )

  val query = from(users)
    .leftJoin(accounts on employees.manager_id === managers.id)
    .select(userParser.columns:_*)
    .map(userParser)

  val res: Seq[User] = query(con) 

