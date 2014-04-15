using (tables.users) {u =>
  ...
  u.id           // Selection[Int] 
  u.name         // Selection[String]
  u.created_date // Selection[java.sql.Timestamp]
  ...
}
