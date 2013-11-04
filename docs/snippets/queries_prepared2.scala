val stmt = conn.prepareStatement("""
  SELECT u.id, u.email
  FROM users u
  WHERE u.first_name LIKE ? AND
    u.age > ? AND
    u.created_at <= ?
""")
stmt.setString(1, "alvar%")
stmt.setInt(2, 30)
stmt.setTimestamp(3, new java.sql.Timestamp(DateTime.now.toMillis))
...

