package com.gravitydev.scoop
package query

import java.sql.{Connection, ResultSet}
import ast.{SqlType, SqlRawExpr, SqlNullableCol, SqlNonNullableCol}

/** A SQL query (or fragment) along with parameters, that can be appended to another or executed if complete */
final case class ParameterizedSql (sql: String, params: Seq[SqlParam[_]] = Nil) {
  def +~ (s: ParameterizedSql): ParameterizedSql = new ParameterizedSql(sql + s.sql, params ++ s.params)
  def onParams (p: SqlParam[_]*) = copy(params = this.params ++ p) 
  def %? (p: SqlParam[_]*) = onParams(p:_*)
  //def as (alias: String) = new AliasedSqlFragment(this, alias)
  override def toString = getClass.getName + "ParameterizedSql(sql="+sql+", params="+params+")" 

  @deprecated("Use process", "0.2.6-SNAPSHOT")
  def map [B](process: ResultSet => ParseResult[B])(implicit c: Connection): List[B] = executeQuery(this)(process).toList

  def process [B](rowParser: ResultSet => ParseResult[B])(implicit c: Connection): util.QueryResult[B] = new util.QueryResult(executeQuery(this)(rowParser))

  def union (q: ParameterizedSql) = (sql + "\n UNION \n" + q.sql) onParams (params ++ q.params :_*)  
  
  def executeUpdate ()(implicit c: Connection) = try util.using(c.prepareStatement(sql)) {stmt => 
    for ((p, idx) <- params.zipWithIndex) p(stmt, idx+1)
    stmt.executeUpdate()
  } catch {
    case e: java.sql.SQLException => throw new Exception("SQL Exception ["+e.getMessage+"] when executing query ["+sql+"] with parameters: ["+params+"]")
  }
}
object ParameterizedSql {
  implicit def fromExpr (expr: ast.SqlExpr[_])(implicit dialect: SqlDialect): ParameterizedSql = dialect.toParameterizedSql(expr)
}

