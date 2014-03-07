val users = 
  from(u)
    .where(
      (u.first_name like "alvar%") && 
      u.age > 30 &&
      u.created_at <= DateTime.now
    )
    .find(u.id ~ u.email)
    .list
