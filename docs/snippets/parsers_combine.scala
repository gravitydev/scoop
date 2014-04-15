import com.gravitydev.scoop._, query._

// type will be List[(Int,String)]
val result = 
  from("users u")
    .select("u.id as id, u.first_name as name")
    .process(sqlLong("userId") ~ sqlString("name"))
    .list
