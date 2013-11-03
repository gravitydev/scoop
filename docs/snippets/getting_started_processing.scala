// u.id is a Parser[Long]
val ids: List[Long] = from(u).find(u.id)

// use "~" to combine parsers
val users: List[(Long,String)] =
  from(u)
    .find(u.id ~ u.name)

// use ">>" to transform a parser
val users: List[String] =
  from(u)
    .find(
      u.first_name ~ u.last_name ~ u.age >> {(first,last,age) => s"$first $last ($age)"}
    )

// use a case classes
case class Employee(first: String, age: Int)
val users: List[Employee] =
  from(u)
    .find(
      u.first ~ u.age >> Employee.apply
    )

// nest parsers
case class User(id: String, email: String)
case class IssueWithReporter(id: Long, reporter: User)
val issues: List[IssueWithReporter] = 
  from(i)
    .innerJoin(u on i.reported_by === u.id)
    .find(
      i.id ~ 
      (u.id ~ u.email >> User.apply) >> Issue.apply
    )

// handle optional data
case class User(id: String, email: String)
case class IssueWithAssignee(id: Long, assignee: Option[User])
val issues: List[IssueWithAssignee] = 
  from(i)
    .leftJoin(u on i.assigned_to === u.id)
    .find(
      i.id ~ 
      opt(u.id ~ u.email >> User.apply) >> Issue.apply
    )

// predefine re-usable parsers
case class User(id: Long, email: String)
case class Project(id: Long, name: String, creator: User)
object Parsers {
  // given a reference to the users table, define a parser for a user
  def user (u: tables.users) = 
    u.id ~ u.email >> User.apply

  // given a reference to the projects and users tables,
  // define a parser for a project that also parses the creator
  def project (p: tables.projects, u: tables.users) =
    p.id ~ user(u) >> Project.apply
}

// use the parsers
val projects: List[Project] = 
  from(p)
    .innerJoin(u on p.created_by === u.id)
    .find(Parsers.project(p, u))

