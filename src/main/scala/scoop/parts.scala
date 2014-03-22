package com.gravitydev.scoop
package query

import java.sql.{Connection, ResultSet}
import ast.{SqlType, SqlNamedExpr, SqlNamedExprImpl, SqlRawExpr, SqlNullableCol, SqlNonNullableCol}

/**
 * A string portion of a query along with its parameters
 */
sealed class SqlS (val sql: String, val params: List[SqlParam[_]] = Nil) {
  override def toString = getClass.getName + "(sql="+sql+", params="+params+")" 
}
object SqlS {
  implicit def fromExpr(expr: ast.SqlExpr[_]) = new SqlS(expr.sql, expr.params)
}

/**
 * A portion of a query that can be appended to another
 */
sealed class SqlFragmentS (sql: String, params: List[SqlParam[_]] = Nil) extends SqlS(sql,params) {
  def +~ (s: SqlFragmentS): SqlFragmentS = new SqlFragmentS(sql + s.sql, params ++ s.params)
  def onParams (p: SqlParam[_]*) = new SqlFragmentS(sql, params ++ p.toList)
  def %? (p: SqlParam[_]*) = onParams(p:_*)
  def as (alias: String) = new AliasedSqlFragmentS(sql, alias, params)
}
object SqlFragmentS {
  implicit def fromExpr (expr: ast.SqlExpr[_]) = new SqlFragmentS(expr.sql, expr.params)
}

class AliasedSqlFragmentS (_sql: String, val name: String, params: List[SqlParam[_]] = Nil) 
    extends SqlS("(" + util.formatSubExpr(_sql) + ")", params) with ast.SqlNamed {

  def selectSql = sql + " as " + name
 
  def apply [X:SqlType](col: SqlNonNullableCol[X]): SqlNamedExpr[X,X] = new SqlNamedExprImpl[X](
    name    = col.name,
    exprSql     = name+"."+col.name,
    params  = col.params
  )

  def on (pred: ast.SqlExpr[Boolean]) = Join(selectSql, pred.sql, params ++ pred.params)
}

// TODO: is this even necessary anymore?
class ExprS (s: String, params: List[SqlParam[_]] = Nil) extends SqlFragmentS(s, params) 
object ExprS {
  implicit def fromExpr (expr: ast.SqlExpr[_]) = new ExprS(expr.sql, expr.params)
  implicit def fromString (s: String)       = new ExprS(s)
  implicit def fromCol (col: ast.SqlCol[_]) = new ExprS(col.sql)
}

/**
 * Same as ExprS, but it might be aliased
 */
class SelectExprS     (s: String, params: List[SqlParam[_]] = Nil) extends SqlS(s, params)
object SelectExprS {
  implicit def fromString (s: String)           = new SelectExprS(s)
  implicit def fromFragment (s: SqlFragmentS)   = new SelectExprS(s.sql, s.params)
  implicit def fromNamed (expr: ast.SqlNamedExpr[_,_]) = new SelectExprS(expr.selectSql, expr.params)
  implicit def fromExprS (expr: ExprS)          = new SelectExprS(expr.sql, expr.params)
}

class FromS (s: String, params: List[SqlParam[_]] = Nil) extends SqlS(s, params) 
object FromS {
  implicit def fromString (s: String) = new FromS(s)
  implicit def fromTable (t: ast.SqlTable[_]) = new FromS(t.sql)
  implicit def fromNamedQuery (q: ast.SqlNamedQuery) = new FromS(q.sql, q.params)
  implicit def fromAliasedSql (q: AliasedSqlFragmentS) = new FromS(q.sql, q.params)
}

class UpdateQueryableS (s: String) extends SqlS(s)
object UpdateQueryableS {
  implicit def fromTable(t: ast.SqlTable[_]) = new UpdateQueryableS(t.sql)
}

class JoinS (s: String, params: List[SqlParam[_]]) extends SqlS(s, params)
object JoinS {
  implicit def strToJoin (s: String)         = new JoinS(s, Nil)
  implicit def fromJoin (j: Join)            = new JoinS(j.sql, j.params)
  //implicit def fromAliasSqlFragmentS (s: AliasedSqlFragmentS) = new JoinS(s.selectSql, s.params)
}

class PredicateS (s: String, params: List[SqlParam[_]]) extends SqlS(s, params)
object PredicateS {
  implicit def fromFragment (f: SqlFragmentS) = new PredicateS(f.sql, f.params)
  implicit def fromString (s: String)    = new PredicateS(s, Nil)
  implicit def fromPredicate (pred: SqlExpr[Boolean]) = new PredicateS(pred.sql, pred.params)
  implicit def fromQueryS (p: QueryS) = new PredicateS(p.sql, p.params)
}


class OrderByS   (s: String, params: List[SqlParam[_]]) extends SqlS(s, params)
object OrderByS {
  implicit def fromExpr(expr: ast.SqlExpr[_]) = fromOrdering(expr.asc)
  implicit def fromOrdering(ord: ast.SqlOrdering) = new OrderByS(ord.sql, ord.params)
  implicit def fromString (s: String)        = new OrderByS(s, Nil)
  
}
class AssignmentS (s: String, params: List[SqlParam[_]]) extends SqlS(s, params)

/**
 * A query as a string, ready to be executed 
 * TODO: Move this somewhere else
 */
class QueryS (s: String, params: List[SqlParam[_]]) extends SqlFragmentS(s, params) {
  @deprecated("Use process", "0.2.6-SNAPSHOT")
  def map [B](process: ResultSet => ParseResult[B])(implicit c: Connection): List[B] = executeQuery(this)(process).toList

  def process [B](rowParser: ResultSet => ParseResult[B])(implicit c: Connection): util.QueryResult[B] = new util.QueryResult(executeQuery(this)(rowParser))
  
  def executeUpdate ()(implicit c: Connection) = try util.using(c.prepareStatement(sql)) {stmt => 
    for ((p, idx) <- params.zipWithIndex) p(stmt, idx+1)
    stmt.executeUpdate()
  } catch {
    case e: java.sql.SQLException => throw new Exception("SQL Exception ["+e.getMessage+"] when executing query ["+sql+"] with parameters: ["+params+"]")
  }
}

