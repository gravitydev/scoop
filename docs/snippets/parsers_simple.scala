val parseInt: ResultSet => Either[String,Int] = rs => Right(rs.getInt(1))
