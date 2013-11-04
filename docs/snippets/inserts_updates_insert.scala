val id: Option[Long] =
  insertInto(u)
    .values(
      u.first_name := "Alvaro",
      u.last_name := "Carrasco",
      u.age := 31
    )() // note the extra parens
