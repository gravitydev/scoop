package my.code.data

import com.gravitydev.scoop._

object tables {
  class users extends Table[users] (users) {
    val id          = col[Long]           ('id)
    val first_name  = col[String]         ('first_name)
    val last_name   = col[String]         ('last_name)
    val age         = col[Int]            ('age)
    val nickname    = col[String]         ('nickname)    nullable
  }

  class projects extends Table[projects] (projects) {
    val id          = col[Long]           ('id)
    val name        = col[String]         ('name)
    val created_by  = col[Long]           ('creator_id)
  }

  class issues extends Table[issues] (issues) {
    val id          = col[Long]           ('id)
    val summary     = col[String]         ('summary)
    val reported_by = col[Long]           ('reported_by)
    val assigned_to = col[Long]           ('assigned_to)  nullable
  }
}

