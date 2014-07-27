package sample

import com.gravitydev.scoop._, query._
import Models._
import Data._

object Parsers {
  def user (u: tables.users) = 
    u.id ~ u.first_name ~ u.last_name ~ u.role >> {(i,f,l,r) => 
      User(i, f+" "+l, r)
    }

  // cleaner
  def user2 (u: tables.users) = 
    u.id ~
    (u.first_name ~ u.last_name >> (_ + " " + _)) ~
    u.role >> User.apply

  def total (count: SqlExpr[Long]) = count //>> (x => x)
 
  //val rep = user(users)
  
  //def issue (i: Issues, r: Users, a: Users) = i.id ~ i.status ~ user(r).as(prefix="reporter_") ~ opt(user(a).as(prefix="assignee_")) map {(i,s,rep,assignee) => Issue(i,s,rep,assignee)}
}

