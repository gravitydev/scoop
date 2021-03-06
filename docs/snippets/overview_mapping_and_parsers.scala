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
  // make sure the case class has the correct number and 
  // type of parameters
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
val users: List[User] = using (tables.users, tables.accounts) {(u, a) =>
  from(u)
    .leftJoin(a on u.account_id === a.id)
    .find(Parsers.user(u,a))
}

