import play.api.Play.current
import play.api.db.DB
...
val users = DB.withTransaction {implicit conn => // provided by play
  using (tables.users) {u => // create aliases
    from(u)
      .where(u.age > 20 && u.activated_at.isNotNull)
      .orderBy(u.name, u.age desc)
      .find(u.id ~ u.name)
  }
}
...
DB.withTransaction {implicit conn =>
  // basic insert
  val id: Option[Long] = 
    insertInto(u)
      .values(
        u.first_name := "Alvaro",
        u.last_name := "Carrasco",
        u.age := 31 
      )()

  // basic update
  update(u)
    .set(u.first_name := "Alvaro", u.age := 31)
    .where(u.id === 1L)() // note the extra parens

}
...
