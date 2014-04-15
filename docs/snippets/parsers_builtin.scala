import com.gravitydev.scoop._

// type will be List[Int]
val ids = 
  from("users u")
    .select("u.id as userId")
    .process(sqlLong("userId"))
    .list

// type will be List[String]
val names = 
  from("users u")
    .select("u.name as name")
    .process(sqlString("name"))
    .list
