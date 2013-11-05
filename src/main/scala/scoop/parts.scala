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
  def onParams (p: SqlParam[_]*) = new SqlFragmentS(sql, params ++ p.toSeq)
  def %? (p: SqlParam[_]*) = onParams(p:_*)
  def as (alias: String) = new AliasedSqlFragmentS(sql, alias, params)
}
object SqlFragmentS {
  implicit def fromExpr (expr: ast.SqlExpr[_]) = new SqlFragmentS(expr.sql, expr.params)
}

class AliasedSqlFragmentS (_sql: String, alias: String, params: Seq[SqlParam[_]] = Seq()) 
    extends SqlS("(" + util.formatSubExpr(_sql) + ") as " + alias, params) {
 
  // generate a column alias
  def apply [X:SqlType](column: String) = new ast.SqlRawExpr[X](alias+"."+column)
  def apply [X:SqlType](col: ast.SqlNamedExpr[X]) = new ast.SqlRawExpr[X](alias+"."+col.name).as(col.name)

  def apply [X:SqlType](col: ast.SqlNonNullableCol[X]) = new ast.SqlNamedReqExpr[X](
    name    = col.name,
    sql     = alias+"."+col.name,
    params  = col.params
  )
  def apply [X:SqlType](col: ast.SqlNullableCol[X]) = new ast.SqlNamedOptExpr[X](
    name    = col.name,
    sql     = alias+"."+col.name,
    params  = col.params
  )

  def on (pred: ast.SqlExpr[Boolean]) = Join(this.sql, pred.sql, params ++ pred.params)
}

// TODO: is this even necessary anymore?
class ExprS (s: String, params: Seq[SqlParam[_]] = Seq()) extends SqlFragmentS(s, params) 
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
  implicit def fromFragment (s: SqlFragmentS)   = new SelectExprS(s.sql, s.params)
  implicit def fromNamed (expr: ast.SqlNamedExpr[_]) = new SelectExprS(expr.sql, expr.params)
  implicit def fromExprS (expr: ExprS)          = new SelectExprS(expr.sql, expr.params)
  implicit def fromAliased (a: AliasedSqlFragmentS) = new SelectExprS(a.sql, a.params) 
  implicit def fromCol (a: ast.SqlCol[_])       = new SelectExprS(a.sql + " as " + a.name, a.params)
}

class FromS           (s: String, params: Seq[SqlParam[_]] = Seq()) extends SqlS(s, params) 
object FromS {
  implicit def fromString (s: String) = new FromS(s)
  implicit def fromTable (t: ast.SqlTable[_]) = new FromS(t.sql)
  implicit def fromAliasSqlFragmentS (s: AliasedSqlFragmentS) = new FromS(s.sql, s.params)
  implicit def fromNamedQueryExpr (q: ast.SqlNamedQueryExpr[_]) = new FromS(q.sql, q.params)
}

class UpdateQueryableS (s: String) extends SqlS(s)
class JoinS           (s: String, params: Seq[SqlParam[_]]) extends SqlS(s, params)

class PredicateS (s: String, params: Seq[SqlParam[_]]) extends SqlS(s, params)
object PredicateS {
  implicit def fromFragment (f: SqlFragmentS) = new PredicateS(f.sql, f.params)
}


class OrderByS   (s: String, params: Seq[SqlParam[_]]) extends SqlS(s, params)
object OrderByS {
  implicit def fromExpr(expr: ast.SqlExpr[_]) = fromOrdering(expr.asc)
  implicit def fromOrdering(ord: ast.SqlOrdering) = new OrderByS(ord.sql, ord.params)
}
class AssignmentS (s: String, params: Seq[SqlParam[_]]) extends SqlS(s, params)

/**
 * A query as a string, ready to be executed 
 */
class QueryS (s: String, params: Seq[SqlParam[_]]) extends SqlFragmentS(s, params) {
  def map [B](process: ResultSet => ParseResult[B])(implicit c: Connection): List[B] = executeQuery(this)(process)
  //def +~ (s: SqlS) = new QueryS(sql + s.sql, params ++ s.params)
  
  def executeUpdate ()(implicit c: Connection) = try util.using(c.prepareStatement(sql)) {stmt => 
    for ((p, idx) <- params.zipWithIndex) p(stmt, idx+1)
    stmt.executeUpdate()
  } catch {
    case e: java.sql.SQLException => throw new Exception("SQL Exception ["+e.getMessage+"] when executing query ["+sql+"] with parameters: ["+params+"]")
  }
}

