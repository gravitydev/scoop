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
