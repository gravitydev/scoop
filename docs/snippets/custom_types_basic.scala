package my.code.data

// place implicits on a package object so they get imported
// when you do "import my.code.data._"
object `package` {
  implicit object issueStatusT extends SqlCustomType[IssueStatus, String] (
    // String => IssueStatus
    key => key match {
      case "open"   => Open 
      case "closed" => Closed
    },
    // IssueStatus => String
    status => status match {
      case Open   => "open"
      case Closed => "closed"if (status == Open)
    }
  )
}

...

// now you can use your custom type in your data model
case class issues extends Table[issues](issues) {
  ... 
  val status = col[IssueStatus]('status)
  ...
}

...

// and in your queries
val issues: List[(Long, IssueStatus)] = 
  from(i)
    .where(i.status === Open)
    .find(i.id ~ i.status)

