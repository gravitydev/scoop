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

