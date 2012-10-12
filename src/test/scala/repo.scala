import com.gravitydev.scoop._

object Repo {
  
  implicit object SqlIssueStatus extends SqlCustomType [IssueStatus, Int] (IssueStatus.apply _, _.id)
  
  case class Issues (as: String = "i") extends Table[Issues]("issues", Issues) {
    val id          = col[Long]         ("id")
    val project_id  = col[Long]         ("project_id")
    val item_id     = col[Long]         ("item_id")
    val title       = col[String]       ("title")
    val description = col[String]       ("description")
    val status      = col[IssueStatus]  ("status")
    val reported_by = col[Long]         ("reported_by")
    val assigned_to = col[Long]         ("assigned_to")   nullable
    val release_id  = col[Long]         ("release_id")    nullable
    
    val * = id ~ project_id
  }
  def issues = Issues("i")
  
  case class Users (as: String) extends Table[Users]("users", Users) {
    val id          = col[Long]         ("id")
    val first_name  = col[String]       ("first_name")
    val last_name   = col[String]       ("last_name")
    val email       = col[String]       ("email")
  }
  def users = Users("u")
  
  
  
  
  
  
  
  
  
  case class IssueStatus(id: Int)
  
  case class User (
    id:   Long,
    name: String
  )
  
  case class Issue (
    id:       Long,
    status:   IssueStatus,
    reporter: User,
    assignee: Option[User]
  )
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  object Parsers {
    def user (u: Users) = u.id ~ u.first_name ~ u.last_name map {case i~f~l => User(i,f+" "+l)}
    
    def issue (i: Issues, r: Users, a: Users) = i.id ~ i.status ~ user(r).as(prefix="reporter_") ~ opt(user(a).as(prefix="assignee_")) map {case i~s~rep~assignee => Issue(i,s,rep,assignee)}
  }
  
}
