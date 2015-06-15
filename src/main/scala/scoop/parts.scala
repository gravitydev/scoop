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

  def on (pred: ast.SqlExpr[Boolean]) = new builder.JoinBuilder(selectSql, pred)
}

/**
 * Might be aliased
 */
class SelectExprS     (s: String, params: List[SqlParam[_]] = Nil) extends SqlS(s, params)
object SelectExprS {
  implicit def fromFragment (s: SqlFragmentS)   = new SelectExprS(s.sql, s.params)
  implicit def fromNamed (expr: ast.SqlNamedExpr[_,_]) = new SelectExprS(expr.selectSql, expr.params)
  implicit def fromExpr (expr: ast.SqlExpr[_]) = new SelectExprS(expr.sql, expr.params)
}

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

