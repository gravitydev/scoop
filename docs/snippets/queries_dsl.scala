val users = 
  from(u)
    .innerJoin(...)
    .leftJoin(...)
    .where(...)
    .orderBy(...)
    .limit(...)
    .offset(...)
    .find(...)
    .list // or head, headOption, etc
