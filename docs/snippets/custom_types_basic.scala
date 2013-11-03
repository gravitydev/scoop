package my.code.models

// some custom enum-like structure (use your own)
sealed abstract class IssueStatus (name: String)
object IssueStatus {
  case object Open    extends IssueStatus("open")
  case object Closed  extends IssueStatus("closed")

  def forName (name: String) = name match {
    case "open"   => Open
    case "closed" => Closed
  }
}

...

package my.code.data

implicit object issueStatusT extends SqlCustomType[IssueStatus, String] (
  IssueStatus forName _, // String => IssueStatus
  _.name                 // IssueStatus => String
)

...

// now you can use your custom type in your data model
case class issues extends Table[issues](issues) {
  ... 
  val status = col[Status]('status)
  ...
}

...

// and in your queries
val issues: List[(Long, IssueStatus)] = 
  from(i)
    .where(i.status === Open)
    .find(i.id ~ i.status)

