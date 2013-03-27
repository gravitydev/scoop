.. _squeryl_comparison

##################
Squeryl Comparison
##################


The data model represents the rows (the data) as case classes

+-----------------------------------------------------------+---------------------------------------------+
| Scoop                                                     | Squeryl                                     |
+-----------------------------------------------------------+---------------------------------------------+
| The data model represents the database tables and columns | The data model                              |
|                                                           |                                             |
| .. code-block:: scala                                     | .. code-block:: scala                       |
|                                                           |                                             |
|   import com.gravitydev.scoop._                           |   import org.squeryl.PrimitiveTypeMode._    |
|                                                           |                                             |
|   class users extends Table[users](users) {               |   class User(                               |
|     val id        = col[Long]   ('id)                     |     val id: Long,                           |
|     val first     = col[String] ('first_name)             |     val first: String,                      |
|     val last      = col[String] ('last_name)              |     val last: String,                       |
|     val age       = col[Int]    ('age)                    |     val age: Int,                           |
|     val nickname  = col[String] ('nickname)  nullable     |     val nickname: Option[String]            |
|   }                                                       |   )                                         |
|                                                           |                                             |
+-----------------------------------------------------------+---------------------------------------------+



