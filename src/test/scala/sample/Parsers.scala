package sample

import com.gravitydev.scoop._, query._
import Models._
import Data._

object Parsers {
  def user (u: tables.users) = u.id ~ u.first_name ~ u.last_name >> {(i,f,l) => User(i,f+" "+l)}

  def total (count: ExprParser[Long]) = count >> (x => x)
 
  //val rep = user(users)
  
  //def issue (i: Issues, r: Users, a: Users) = i.id ~ i.status ~ user(r).as(prefix="reporter_") ~ opt(user(a).as(prefix="assignee_")) map {(i,s,rep,assignee) => Issue(i,s,rep,assignee)}
}

