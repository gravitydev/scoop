update(u)
  .set(
    u.first_name := "Alvaro",
    u.age := u.age + 1 // reference existing value
  )
  .where(u.id === 30L)()
