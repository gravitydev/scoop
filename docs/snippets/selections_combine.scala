// type will be Selection[(Long, String, Int)]
val sel = u.id ~ u.name ~ u.age

// type will be Selection[User]
val userSel = u.id ~ u.name ~ u.age >> {(id, name, age) => new User(id, name, age)}

// if User is a case class, you can use its apply function
// type will still be Selection[User]
val userSel = u.id ~ u.name ~ u.age >> User.apply
