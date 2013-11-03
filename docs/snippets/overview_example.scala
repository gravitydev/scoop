val users = List[(Long, String, Long, String)]
  using (tables.users, tables.accounts, tables.users) {(u, a, m) =>
    from(u)
      .innerJoin(a on u.account_id === a.id)
      .leftJoin(m on u.manager_id === m.id)
      .where(u.id > 1000L && a.expire_date > DateTime.now.toLocalDate)
      .groupBy(a.id, m.id)
      .limit(100)
      .offset(10)
      .find(
        a.id ~ a.name ~ 
        countDistinct(u.id) ~
        from(m).where(m.status === Active).select(u.name).as("main")
      )
  }
