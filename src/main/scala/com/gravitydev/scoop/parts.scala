package com.gravitydev.scoop
package query

import java.sql.{Connection, ResultSet}

/**
 * A string portion of a query along with its parameters
 */
sealed class SqlS (val sql: String, val params: Seq[SqlParam[_]] = Seq()) {
  override def toString = getClass.getName + "(sql="+sql+", params="+params+")" 
}
object SqlS {
  implicit def fromExpr(expr: ast.SqlExpr[_]) = new SqlS(expr.sql, expr.params)
}

/**
 * A portion of a query that can be appended to another
 */
sealed class SqlFragmentS (sql: String, params: Seq[SqlParam[_]] = Seq()) extends SqlS(sql,params) {
  def +~ (s: SqlFragmentS) = new SqlFragmentS(sql + s.sql, params ++ s.params)
  def onParams (p: SqlParam[_]*): PredicateS = new PredicateS(sql, params ++ p.toSeq)
  def %? (p: SqlParam[_]*) = onParams(p:_*)
}
object SqlFragmentS {
  implicit def fromExpr (expr: ast.SqlExpr[_]) = new SqlFragmentS(expr.sql, expr.params)
}

class ExprS (s: String, params: Seq[SqlParam[_]] = Seq()) extends SqlFragmentS(s, params) {
  def as (alias: String) = new SelectExprS(s + " as " + alias, params)
}
object ExprS {
  implicit def fromString (s: String)       = new ExprS(s)
  implicit def fromCol (col: ast.SqlCol[_]) = new ExprS(col.sql)
}

/**
 * Same as ExprS, but it might be aliased
 */
class SelectExprS     (s: String, params: Seq[SqlParam[_]] = Nil) extends SqlS(s, params)
object SelectExprS {
  implicit def fromString (s: String)           = new SelectExprS(s)
  implicit def fromCol (col: ast.SqlCol[_])     = new SelectExprS(col.selectSql)
  implicit def fromExprS (expr: ExprS)          = new SelectExprS(expr.sql, expr.params)
  implicit def fromExpr (expr: ast.SqlExpr[_])  = new SelectExprS(expr.sql, expr.params)
}

class FromS           (s: String) extends SqlS(s) 
object FromS {
  implicit def fromString (s: String) = new FromS(s)
  implicit def fromTable (t: ast.SqlTable[_]) = new FromS(t.sql)
}

class UpdateQueryableS (s: String) extends SqlS(s)
class JoinS           (s: String, params: Seq[SqlParam[_]]) extends SqlS(s, params)

class PredicateS (s: String, params: Seq[SqlParam[_]]) extends SqlS(s, params)
object PredicateS {
  implicit def fromFragment (f: SqlFragmentS) = new PredicateS(f.sql, f.params)
}


class OrderByS   (s: String) extends SqlS(s)
class AssignmentS (s: String, params: Seq[SqlParam[_]]) extends SqlS(s, params)

/**
 * A query as a string, ready to be executed 
 */
class QueryS (s: String, params: Seq[SqlParam[_]]) extends SqlFragmentS(s, params) {
  def map [B](process: ResultSet => ParseResult[B])(implicit c: Connection): List[B] = executeQuery(this)(process)
  //def +~ (s: SqlS) = new QueryS(sql + s.sql, params ++ s.params)
}
