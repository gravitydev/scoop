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
  def account (a: accounts) =
    a.id ~ a.name >> Account.apply

  // you can use an existing parser when defining a new one:
  def user (u: users, a: accounts) =
    u.id ~ u.first_name ~ u.last_name ~ opt(account(a)) >> User.apply
}

// instantiate a parser by specifying the tables it should use
// the tables can be configured with aliases
// this makes the definition of the parser general,
// but the instantiation specific to the query and the aliases used
val userParser = Parsers.user( users, accounts )

val query = from(users)
  .leftJoin(accounts on employees.manager_id === managers.id)
  .select(userParser.columns:_*)
  .map(userParser)

val res: Seq[User] = query(con)
