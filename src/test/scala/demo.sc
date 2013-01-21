import com.gravitydev.scoop._, query._
import java.sql.Connection
import Repo._

object demo {
  implicit val con = java.sql.DriverManager.getConnection("jdbc:mysql://localhost/gravitydev", "root", "")
                                                  //> con  : java.sql.Connection = com.mysql.jdbc.JDBC4Connection@b41b571
  
  
  
  
  
  
  
  
  
  

  val u = users as "u"                            //> u  : Repo.Users = Users(u)
  val u2 = users as "u2"                          //> u2  : Repo.Users = Users(u2)
  val i = issues as "i"                           //> i  : Repo.Issues = Issues(i)
  
  (u2.first_name === "test").sql                  //> res0: java.lang.String = (u2.first_name = ?)
  
  
  val parser = Parsers.issue(i, u, u2)            //> parser  : com.gravitydev.scoop.Parser[Repo.Issue] = <function1>
  
  val x = from(i)
    .innerJoin(u on i.reported_by === u.id)
    .leftJoin(u2 on i.assigned_to === u2.id)
    .where(i.assigned_to isNotNull)
    .select(parser.columns:_*) map parser map println
                                                  //> Issue(8,IssueStatus(2),User(4,Alvaro Carrasco),Some(User(4,Alvaro Carrasco))
                                                  //| )
                                                  //| Issue(9,IssueStatus(2),User(23,Scott Worley),Some(User(21,Chad Walling)))
                                                  //| Issue(10,IssueStatus(2),User(4,Alvaro Carrasco),Some(User(4,Alvaro Carrasco)
                                                  //| ))
                                                  //| Issue(11,IssueStatus(1),User(4,Alvaro Carrasco),Some(User(4,Alvaro Carrasco)
                                                  //| ))
                                                  //| Issue(13,IssueStatus(2),User(39,Bill Wise),Some(User(4,Alvaro Carrasco)))
                                                  //| Issue(15,IssueStatus(2),User(17,Nicole Neumarker),Some(User(11,Jennifer Char
                                                  //| rey)))
                                                  //| Issue(16,IssueStatus(2),User(17,Nicole Neumarker),Some(User(4,Alvaro Carrasc
                                                  //| o)))
                                                  //| Issue(17,IssueStatus(2),User(17,Nicole Neumarker),Some(User(11,Jennifer Char
                                                  //| rey)))
                                                  //| Issue(18,IssueStatus(2),User(17,Nicole Neumarker),Some(User(4,Alvaro Carrasc
                                                  //| Output exceeds cutoff limit. 
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
}
