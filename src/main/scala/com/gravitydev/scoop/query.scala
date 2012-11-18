package com.gravitydev.scoop
package query

import java.sql.{Connection, Date, Timestamp, ResultSet}
import scala.collection.mutable.ListBuffer
import collection._, ast._

object `package` {
  
  implicit def tableToWrapped [T <: SqlTable[_]] (t: T) = new TableWrapper(t)
  implicit def baseToSqlLit [T](base: T)(implicit sqlType: SqlType[T]) = SqlLiteralExpr(base)
  implicit def baseToParam [T](base: T)(implicit sqlType: SqlType[T]) = SqlSingleParam(base)

  implicit def toFrom (s: String)         = new FromS(s)
  implicit def toExpr (s: String)         = new ExprS(s)
  implicit def toJoin (s: String)         = new JoinS(s, Nil)
  implicit def toPredicate (s: String)    = new PredicateS(s, Nil)
  implicit def toOrder (s: String)        = new OrderByS(s)
  
  implicit def colToExprS (col: SqlCol[_])  = new ExprS(col.selectSql)
  implicit def tableToFrom (t: SqlTable[_]) = new FromS(t.sql)
  implicit def joinToJoin (j: Join)         = new JoinS(j.sql, j.params)
  implicit def predToPredicateS (pred: SqlExpr[Boolean]) = new PredicateS(pred.sql, pred.params)
  implicit def orderingToOrder (o: SqlOrdering) = new OrderByS(o.sql)
  implicit def assignmentToAssignmentS (a: SqlAssignment[_]) = new AssignmentS(a.sql, a.params)
  
  implicit def listToExpr (l: List[String]) = l.map(x => x: ExprS)

  // starting point
  def from (table: FromS) = Query(table.sql)
  def insertInto (table: SqlTable[_]) = new InsertBuilder(table.tableName)
}

class TableWrapper [T <: SqlTable[_]](t: T) {
  def on (pred: SqlExpr[Boolean]) = Join(t.sql, pred.sql, pred.params)
}

case class Join (table: String, predicate: String, params: Seq[SqlParam[_]]) {
  def sql = table + " ON " + predicate
}

sealed abstract class SqlS (val sql: String) {
  override def toString = getClass.getName + "(" + sql + ")"
}
class ExprS      (s: String) extends SqlS(s)
class FromS      (s: String) extends SqlS(s)
class JoinS      (s: String, val params: Seq[SqlParam[_]]) extends SqlS(s)
class PredicateS (s: String, val params: Seq[SqlParam[_]]) extends SqlS(s)
class OrderByS   (s: String) extends SqlS(s)
class AssignmentS (s: String, val params: Seq[SqlParam[_]]) extends SqlS(s)

class InsertBuilder (into: String) {
  def set (assignments: AssignmentS*) = Insert (into, assignments.map(_.sql).toList)
}

case class Insert (
  into: String,
  assignments: List[String] = Nil
) {
  def sql = "INSERT INTO " + into + " SET " + assignments.mkString(", ")
  def apply ()(implicit c: Connection) = {
    util.using(c.prepareStatement(sql)) {stmt => 
      stmt.executeUpdate()
    }
    
    util.using(c.prepareStatement("SELECT LAST_INSERT_ID()")) {stmt => 
      util.using(stmt.executeQuery()) (_ getLong 1)
    }
  }
}

case class Update (
  table: String,
  assignments: List[String] = Nil,
  predicate: Option[String] = None
)

case class Query (
  from:       String,
  sel:        List[String]    = List("*"),
  joins:      List[String]    = Nil,
  predicate:  Option[String]  = None,
  order:      Option[String]  = None,
  group:      List[String]    = Nil,
  params:     Seq[SqlParam[_]] = Nil,
  limit:      Option[Int]     = None,
  offset:     Option[Int]     = None
) {
  def select (cols: ExprS*)   = copy(sel = cols.map(_.sql).toList)
  def addCols (cols: ExprS*)  = copy(sel = sel ++ cols.map(_.sql).toList)
  def innerJoin (join: JoinS) = copy(joins = joins ++ List("INNER JOIN " + join.sql), params = this.params ++ join.params )
  def leftJoin (join: JoinS)  = copy(joins = joins ++ List("LEFT JOIN " + join.sql), params = this.params ++ join.params)
  
  def where (predicate: PredicateS, params: SqlSingleParam[_,_]*) = copy(predicate = Some(predicate.sql)/*, params = this.params ++ predicate.params ++ params.toList*/)
  def where (predicate: SqlExpr[Boolean]) = copy(predicate = Some(predicate.sql), params = this.params ++ predicate.params)
  def addWhere (pred: PredicateS, params: SqlSingleParam[_,_]*) = copy(predicate = predicate.map(_ + " AND " + pred.sql).orElse(Some(pred.sql)), params = this.params ++ params.toSeq)
  def orderBy (order: OrderByS*) = copy(order = Some( (order.toList.map(_.sql)).mkString(", ")) )
  def groupBy (cols: ExprS*) = copy(group = cols.map(_.sql).toList)
  def limit (l: Int): Query = copy(limit = Some(l))
  def offset (o: Int): Query = copy(offset = Some(o))
  
  def sql = 
    "SELECT " + sel.mkString(", ") + "\n" + 
    "FROM " + from + "\n" +
    joins.mkString("", "\n", "\n") + 
    predicate.map(w => "WHERE " + w + "\n").getOrElse("") + 
    (if (group.nonEmpty) group.mkString("GROUP BY ", ", ", "\n") else "") +
    order.map("ORDER BY " + _ + "\n").getOrElse("") +
    limit.map("LIMIT " + _ + "\n").getOrElse("") +
    offset.map("OFFSET " + _ + "\n").getOrElse("")
  
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
