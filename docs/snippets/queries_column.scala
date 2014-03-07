val ids: List[Long] =
  from(u)
    .find(u.id)
    .list
