Using Joda Date/Time
====================

Using Joda is recommended over the default java date stuff. There is no built-in support since it is really easy to add it with custom types. 

Make sure you have included joda as a dependency (note that you usually need joda-time AND joda-convert). 

Then define a custom mapping like this:

```scala
package my.project.sql

import com.gravitydev.scoop._

object `package` {
  // like with all custom column types, you just need a couple of functions 
  // for going back and forth from a native type to a custom one
  implicit object dateTime  extends SqlCustomType [DateTime, java.sql.Timestamp] (
    d => new DateTime(d.getTime), 
    d => new java.sql.Timestamp(d.getMillis)
  )
  implicit object localDate extends SqlCustomType [LocalDate, java.sql.Date] (
    d => LocalDate.fromDateFields(d), 
    d => new java.sql.Date(d.toDate.getTime)
  )
}

// you can now use joda on your table definitions
object tables {

  class users extends Table[users](users) {
    ... 
    val created_at = col[DateTime] ('created_at)
    ..
  }

  ...
}
```

