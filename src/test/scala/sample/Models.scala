package sample

object Models {

  sealed abstract class IssueStatus(val id: Int)
  object IssueStatuses {
    def forId (x: Int) = if (x==1) Open else Closed
    object Open extends IssueStatus(1)
    object Closed extends IssueStatus(2)
  }
 
  sealed abstract class Role (val role: String)
  object Role {
    def forName (name: String) = name match {
      case "admin" => Admin
      case "spectator" => Spectator
    }
    object Admin extends Role("admin")
    object Spectator extends Role("spectator")
  }
  case class User (
    id:   Long,
    name: String,
    role: Role
  )
  
  case class Issue (
    id:       Long,
    status:   IssueStatus,
    reporter: User,
    assignee: Option[User],
    release_id: Option[Long]
  )
  
}

