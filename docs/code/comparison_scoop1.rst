.. code-block:: scala                                     

 import com.gravitydev.scoop._                    
                                                      
 class users extends Table[users](users) {               
   val id        = col[Long]   ('id)                    
   val first     = col[String] ('first_name)            
   val last      = col[String] ('last_name)             
   val age       = col[Int]    ('age)                 
   val nickname  = col[String] ('nickname)  nullable
 }                                                      
                                              
The data model represents the database tables and columns 
