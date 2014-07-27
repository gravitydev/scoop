package sample

import com.gravitydev.scoop._
import Models._

object Data {
    
  implicit val issueStatusT = customType[IssueStatus, Int](IssueStatuses.forId _, _.id)
  implicit val roleT        = customType[Role, String]    (Role.forName _, _.role)

  object tables {
 
    /** type annotation is not required in scala 2.10 */ 
    case class issues () extends Table[issues](issues, "issues") {
      val id          = col[Long]         ('id)
      val project_id  = col[Long]         ('project_id)
      val item_id     = col[Long]         ('item_id)
      val title       = col[String]       ('title)
      val description = col[String]       ('description)
      val status      = col[IssueStatus]  ('status) // postgres: , cast = "status")
      val reported_by = col[Long]         ('reported_by)
      val assigned_to = col[Long]         ('assigned_to)   nullable
      val release_id  = col[Long]         ('release_id)    nullable
      
      val * = id ~ project_id
    }
    
    case class users () extends Table[users](users, "users") {
      val id          = col[Long]         ('id)
      def first_name  = col[String]       ('first_name)
      def last_name   = col[String]       ('last_name)
      def role        = col[Role]         ('role) 
      def email       = col[String]       ('email)
      def age         = col[Int]          ('age)
      def nickname    = col[String]       ('nickname) nullable
    }
  }
  
}

