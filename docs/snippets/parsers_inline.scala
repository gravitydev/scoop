// type will be List[Int]
val result = 
  from(u) 
    .where(...)
    .select(...)
    .process(rs => Right(rs.getInt(1)))
    .list
