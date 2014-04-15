// Selection[Int]
val sel = sqlInt("num").as("age")

val parser = sqlString("email") // ResultSetParser[String]
val sel2 = parser as "username" // Selection[String]
