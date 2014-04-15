import com.gravitydev.scoop._, query._

// type will be List[String]
val result = 
  from("users u")
    .select("u.id as id, u.first_name as name")
    .process(sqlLong("userId") ~ sqlString("name") >> ((id, name) => s"Name: $name, ID: $id"))
    .list

// you can also define a reusable parser
val parser = 
  sqlLong("userId") ~ sqlString("name") >> {(id, name) => 
    s"Name: $name, ID: $id")
  }

// or build one dynamically
def userDataParser(idAlias: String, nameAlias: String) = 
  sqlLong(idAlias) ~ sqlString(nameAlias) >> {(id, name) =>
    s"Name: $name, ID: $id") 
  }

// and use them like this
val result = 
  .from(...)
  .select(...)
  .process(parser /* or */ userDataParser("id", "name"))
  .list
