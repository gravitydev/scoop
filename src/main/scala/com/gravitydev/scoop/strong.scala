package com.gravitydev.scoop
package strong

import ast._, collection._
import java.sql.Connection

object `package` {
  // shortcuts
  type C = TransactionContext
  type T = Table[_]
  type SqlExprList = KList[SqlExpr, _ <: HList]
  
  def from [A <: T](t1: A) = From1(t1)
  def from [A <: T, B <: T](t1: A, t2: B) = From2(t1, t2)
  
  // boilerplate
  def select [A](x: SqlExpr[A]) = Selection(x :^: KNil)
  def select [A,B](a: SqlExpr[A], b: SqlExpr[B]) = Selection(a :^: b :^: KNil)
  def select [A,B,C](a: SqlExpr[A], b: SqlExpr[B], c: SqlExpr[C]) = Selection(a :^: b :^: c :^: KNil)
}

trait TransactionContext {
  private val names = scala.collection.mutable.Set[String]()

  def uniqueName [Q<:Queryable[_]] (q: Q): Q = synchronized {
    def unique (name: String, number: Int = 1): String = {
      val suffixed = name + (if (number==1) "" else number.toString)
      if (!names.contains(suffixed)) {
        names += suffixed
        suffixed
      } else {
        unique(name, number+1)
      }
    }
    (q as unique(q.as)).asInstanceOf[Q] // for some reason the cast is necessary
  }
}

trait Query [X] extends (Connection => List[X]) {
  //def map [T] (fn: X => T): Query[T]
  //def flatMap [T] (fn: X => Query[T])
}

private [strong] case class Selection [X <: SqlExprList](sel: X)  {
  override def toString = "Selection(" + (sel.toList map (_.sql) mkString(", ")) + ")"
  def sql = "SELECT " + (sel.toList map (_.sql) mkString(", "))
}

trait From extends TransactionContext {
  def sql: String
}

case class From1 [T<:Queryable[_]](q: T) extends From {
  def sql = "FROM " + q.sql
  def apply [X <: SqlExprList](fn: T => Selection[X]) = BaseQuery(this, fn(q))
}
case class From2 [T1<:Queryable[_], T2<: Queryable[_]](q1: T1, q2: T2) extends From {
  lazy val (_q1, _q2) = (uniqueName(q1), uniqueName(q2))
  
  def sql = "FROM " + _q1.sql + ", " + _q2.sql
  def apply [X <: SqlExprList](fn: (T1,T2) => Selection[X]) = {
    BaseQuery(
      this, 
      fn(_q1, _q2)
    )
  }
}

case class BaseQuery [X <: SqlExprList](
  from: From,
  select: Selection[X]
) extends Query [X#HLIST] {
  def sql = select.sql + " \n" + from.sql + " LIMIT 10"
  
  def map [T] (fn: select.sel.HLIST => T) = new MappedQuery(this, fn)
  def apply (con: Connection) = map(x => x)(con)
}

case class MappedQuery [X <: SqlExprList, T] (
  query: BaseQuery[X],
  mapFn: X#HLIST => T
) extends Query [T] {
  def map [V] (fn: T => V) = copy(mapFn = x => fn(mapFn(x)))
  def apply (con: Connection) = {
    implicit val c = con
    util.processQuery(query.sql) {rs =>
      val col = new (SqlExpr ~> Id) {
        def apply [T](x: SqlExpr[T]) = x match {
          case x: SqlCol[_] => x.asInstanceOf[SqlCol[T]](rs) // cast to regain erased type
        }
      }
      mapFn( query.select.sel.down(col) )
    }
  }
}
