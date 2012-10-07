import com.gravitydev.scoop.Table

object Repo {
  case class Issues (as: String = "i") extends Table[Issues]("issues", Issues) {
    val id          = col[Long]         ("id")
    val project_id  = col[Long]         ("project_id")
    val item_id     = col[Long]         ("item_id")
    val title       = col[String]       ("title")
    val description = col[String]       ("description")
    //val status      = col[IssueStatus]  ("status")
    //val reason      = col[IssueReason]  ("reason")
    //val severity    = col[IssueSeverity]("severity")
    //val reported_on = col[DateTime]     ("reported_date")
  }
  def issues = Issues("i")
  
  case class Users (as: String) extends Table[Users]("users", Users) {
    val id          = col[Long]         ("id")
    val first_name  = col[String]       ("first_name")
    val last_name   = col[String]       ("last_name")
    val email       = col[String]       ("email")
  }
  def users = Users("u")
}
