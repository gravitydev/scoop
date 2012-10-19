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
  
  implicit def colToExprS (col: SqlCol[_])  = new ExprS(col.selectSql)
  implicit def tableToFrom (t: SqlTable[_]) = new FromS(t.sql)
  implicit def joinToJoin (j: Join)         = new JoinS(j.sql)
  implicit def predToPredicateS (pred: SqlExpr[Boolean]) = new PredicateS(pred.sql)
  implicit def orderingToOrder (o: SqlOrdering) = new OrderByS(o.sql)
  
  implicit def listToExpr (l: List[String]) = l.map(x => x: ExprS)

  // starting point
  def from (table: FromS) = Query(table)
}

class TableWrapper [T <: SqlTable[_]](t: T) {
  def on (pred: SqlExpr[Boolean]) = Join(t.sql, pred.sql)
}

case class Join (table: String, predicate: String) {
  def sql = table + " ON " + predicate
}

sealed abstract class SqlS (val sql: String) {
  override def toString = getClass.getName + "(" + sql + ")"
}
class ExprS      (s: String) extends SqlS(s)
class FromS      (s: String) extends SqlS(s)
class JoinS      (s: String) extends SqlS(s)
class PredicateS (s: String) extends SqlS(s)
class OrderByS   (s: String) extends SqlS(s)

case class Query (
  from:       FromS,
  sel:       List[ExprS] = List("*": ExprS),
  joins:      List[JoinS] = Nil,
  predicate:  Option[PredicateS] = None,
  order:    Option[OrderByS] = None,
  group:    List[ExprS] = Nil,
  params:     Seq[SqlSingleParam[_,_]] = Nil
) {
  def select (cols: ExprS*)   = copy(sel = cols.toList)
  def addCols (cols: ExprS*)  = copy(sel = sel ++ cols.toList)
  def innerJoin (join: JoinS) = copy(joins = joins ++ List(toJoin("INNER JOIN " + join.sql)))
  def leftJoin (join: JoinS)  = copy(joins = joins ++ List(toJoin("LEFT JOIN " + join.sql)))
  
  def where (predicate: PredicateS, params: SqlSingleParam[_,_]*) = copy(predicate = Some(predicate), params = this.params ++ params.toList)
  def where (predicate: SqlExpr[Boolean]) = copy(predicate = Some(predicate.sql), params = this.params ++ predicate.params)
  def addWhere (pred: PredicateS, params: SqlSingleParam[_,_]*) = copy(predicate = predicate.map(p => toPredicate(p.sql + " AND " + pred.sql)).orElse(Some(pred)), params = this.params ++ params.toSeq)
  def orderBy (order: OrderByS*) = copy(order = Some( toOrder((order.toList.map(_.sql)).mkString(", "))) )
  def groupBy (cols: ExprS*) = copy(group = cols.toList)
  
  def sql = 
    "SELECT " + sel.map(_.sql).mkString(", ") + "\n" + 
    "FROM " + from.sql + "\n" +
    (joins.map(_.sql).mkString("", "\n", "\n")) + 
    predicate.map(w => "WHERE " + w.sql + "\n").getOrElse("") + 
    (if (group.nonEmpty) group.map(_.sql).mkString("GROUP BY ", ", ", "\n") else "") +
    order.map("ORDER BY " + _.sql + "\n").getOrElse("")
  
  def map [B](process: ResultSet => ParseResult[B])(implicit c: Connection): List[B] =
    util.using (c.prepareStatement(sql)) {statement =>
      for ((p, idx) <- params.zipWithIndex) p(statement, idx+1)
      
      util.using (statement.executeQuery()) {results => 
        util.bmap(results.next) { 
          process(results) match {
            case Success(v) => v
            case Failure(e) => error("Scoop Parse Error: " + e)
          }
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