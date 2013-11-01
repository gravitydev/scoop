import com.gravitydev.scoop._

class users extends Table[users](users) {
  val id          = col[Long]           ('id)
  val first_name  = col[String]         ('first_name)
  val last_name   = col[String]         ('last_name)
  val age         = col[Int]            ('age)
  val nickname    = col[String]         ('nickname)    nullable
}

