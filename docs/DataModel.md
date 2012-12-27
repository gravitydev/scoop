Data Model Definition
=====================

You can define your model definition anywhere in your project. You'll just need to import it whenever you make queries. 

At the moment you have to write it by hand, but I'll be soon working on an SBT plugin to generate it automatically from a database.

Here is a sample:

```scala
package my.project.sql

import com.gravitydev.scoop._

// define custom columns types
// using package object, convenient for easy importing, but not required
object `package` {
  
  // custom column type, a very common use case is for using joda date/time types, 
  // but you can make them for anything
  // all you need is a function from YourCustomType to a native JDBC type and vice-versa
  // you can then use your custom type on your definitions
  implicit object issueStatus extends SqlCustomType [IssueStatus, Int] (IssueStatus forId _ get, _.id)
}

// define the tables and their columns
// it is recommended to match the table and column names exactly, but not required
object tables {

  class users extends Table[users](users) {
    val id          = col[Long]           ('id)
    val first_name  = col[String]         ('first_name)
    val last_name   = col[String]         ('last_name)
    val age         = col[Int]            ('age)
    val nickname    = col[String]         ('nickname)    nullable // produces Option[String] when parsing
    val created_at  = col[Timestamp]      ('created_at)
  }

  class issues extends Table[issues](issues) {
    val id          = col[Long]           ('id)
    val title       = col[String]         ('title)
    val description = col[String]         ('description)
    val status      = col[IssueStatus]    ('status)               // note the use of a custom type
    val assigned_to = col[Long]           ('assigned_to) nullable
  }

}
```

