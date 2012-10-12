package com.gravitydev.scoop
package query

import java.sql.{Connection, Date, Timestamp, ResultSet}
import scala.collection.mutable.ListBuffer
import collection._, ast._

object `package` {
  implicit def tableToWrapped [T <: SqlTable[_]] (t: T) = new TableWrapper(t)
  implicit def baseToSqlLit [T](base: T)(implicit sqlType: SqlType[T]) = SqlLiteralExpr(base)

  implicit def toFrom (s: String)         = new FromS(s)
  implicit def toExpr (s: String)         = new ExprS(s)
  implicit def toJoin (s: String)         = new JoinS(s)
  implicit def toPredicate (s: String)    = new PredicateS(s)
  implicit def toOrder (s: String)        = new OrderByS(s)
  
  implicit def colToExprS (col: SqlCol[_])  = new ExprS(col.sql)
  implicit def tableToFrom (t: SqlTable[_]) = new FromS(t.sql)
  implicit def joinToJoin (j: Join)         = new JoinS(j.sql)
  implicit def predToPredicateS (pred: SqlExpr[Boolean]) = new PredicateS(pred.sql)
  implicit def orderingToOrder (o: SqlOrdering) = new OrderByS(o.sql)

  // starting point
  def from (table: FromS) = Query(table)
}

class TableWrapper [T <: SqlTable[_]](t: T) {
  def on (pred: SqlExpr[Boolean]) = Join(t.sql, pred.sql)
}

case class Join (table: String, predicate: String) {
  def sql = table + " ON " + predicate
}

sealed abstract class SqlS (val sql: String)
class ExprS      (s: String) extends SqlS(s)
class FromS      (s: String) extends SqlS(s)
class JoinS      (s: String) extends SqlS(s)
class PredicateS (s: String) extends SqlS(s)
class OrderByS   (s: String) extends SqlS(s)

case class Query (
  from:       FromS,
  cols:       List[ExprS] = List("*": ExprS),
  joins:      List[JoinS] = Nil,
  predicate:  Option[PredicateS] = None,
  orderBy:    Option[OrderByS] = None,
  params:     Seq[SqlSingleParam[_,_]] = Nil
) {
  def select (cols: ExprS*)   = copy(cols = cols.toList)
  def addCols (cols: ExprS*)  = copy(cols = this.cols ++ cols.toList)
  def innerJoin (join: JoinS) = copy(joins = joins ++ List(toJoin("INNER JOIN " + join.sql)))
  def leftJoin (join: JoinS)  = copy(joins = joins ++ List(toJoin("LEFT JOIN " + join.sql)))
  
  def where (predicate: PredicateS, params: SqlSingleParam[_,_]*) = copy(predicate = Some(predicate), params = this.params ++ params.toList)
  def where (predicate: SqlExpr[Boolean]) = copy(predicate = Some(predicate.sql), params = this.params ++ predicate.params)
  //def where (predicate: Predicate) = copy(predicate = Some(predicate.sql), params = this.params ++ predicate.params)
  def addWhere (pred: PredicateS, params: SqlSingleParam[_,_]*) = copy(predicate = predicate.map(p => toPredicate(p.sql + " AND " + pred.sql)).orElse(Some(pred)), params = this.params ++ params.toSeq)
  def orderBy (order: OrderByS*) = copy(orderBy = Some( toOrder((order.toList.map(_.sql)).mkString(", "))) )
  
  def sql = 
    "SELECT " + cols.map(_.sql).mkString(", ") + "\n" + 
    "FROM " + from.sql + "\n" +
    (joins.map(_.sql).mkString("", "\n", "\n")) + 
    predicate.map(w => "WHERE " + w.sql + "\n").getOrElse("") + 
    orderBy.map("ORDER BY " + _.sql + "\n").getOrElse("")
  
  def map [B](process: ResultSet => B)(implicit c: Connection): List[B] =
    util.using (c.prepareStatement(sql)) {statement =>
      for ((p, idx) <- params.zipWithIndex) p(statement, idx+1)
      
      util.using (statement.executeQuery()) {results => 
        util.bmap(results.next) { 
          process(results)
        }
      }
    }
  
  override def toString = {
    "Query(sql="+sql+", params=" + renderParams(params) +")"
  }
  
  //def apply ()(implicit con: Connection) = map(rs => rs.)
  //def single ()(implicit con: Connection) = singleOpt().get
  //def singleOpt ()(implicit con: Connection) = apply().toList.headOption
}

case class OrderBy (order: String, dir: String = null) {
  def sql = order + Option(dir).map(" "+_).getOrElse("")
}

/*
trait Predicate {
  def sql: String
  def params: List[Any]
}
*/