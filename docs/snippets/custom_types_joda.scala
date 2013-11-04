import org.joda.time.{DateTime, LocalDate}
import java.sql.{Date, Timestamp}
...
implicit object dateTimeT extends SqlCustomType [DateTime, Timestamp]  (
  d => new DateTime(d.getTime),   // JDBC => Joda
  d => new Timestamp(d.getMillis) // Joda => JDBC
)
implicit object dateT extends SqlCustomType [LocalDate, Date] (
  d => LocalDate.fromDateFields(d), // JDBC => Joda
  d => new Date(d.toDate.getTime)   // Joda => Jdbc
)

