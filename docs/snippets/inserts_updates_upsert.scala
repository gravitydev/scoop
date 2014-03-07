val id: Option[Long] =
  insertInto(u)
    .set(
      u.first_name := "Alvaro",
      u.last_name := "Carrasco",
      u.age := 31
    )
    .onDuplicateKeyUpdate(
      u.age := u.age + 3
    )() // not the extra parens
