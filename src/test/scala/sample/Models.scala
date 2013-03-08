package sample

object Models {

  sealed abstract class IssueStatus(val id: Int)
  object IssueStatuses {
    def forId (x: Int) = if (x==1) Open else Closed
    object Open extends IssueStatus(1)
    object Closed extends IssueStatus(2)
  }
  
  case class User (
    id:   Long,
    name: String
  )
  
  case class Issue (
    id:       Long,
    status:   IssueStatus,
    reporter: User,
    assignee: Option[User],
    release_id: Option[Long]
  )
  
}
