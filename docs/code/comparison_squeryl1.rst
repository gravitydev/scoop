.. code-block:: scala                       

  import org.squeryl.PrimitiveTypeMode._    

  class User(                               
    val id: Long,                           
    val first: String,                      
    val last: String,                       
    val age: Int,                           
    val nickname: Option[String]            
  )                                         

The data model represents the rows (the data) as case classes

