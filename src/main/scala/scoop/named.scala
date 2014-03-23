package com.gravitydev.scoop
package ast

import query.{Query, Join}
import java.sql.ResultSet

/** 
 * Untyped named sql 
 * Useful for select clause, from clause, etc, but not for parsing since we don't know the type (subtype might)
 */
trait SqlNamed {
  def name: String
  def sql: String
  def params: List[SqlParam[_]] 
}

/** Typed parseable sql expression */
trait SqlNamedExpr [I,+O] extends SqlExpr[I] with SqlNamed with parsers.ExprSelection [O] {self =>
  def parse (rs: ResultSet): Option[O]

  def apply (rs: ResultSet): ParseResult[O] = parse(rs).toRight("Column ["+name+"] not found in ResultSet: "+util.inspectRS(rs))

  def name: String
  def sql: String
  def params: List[SqlParam[_]]

  def selectSql = sql + " as " + name

  def expressions = List(new query.SelectExprS(selectSql, params))

  // this should only be applicable to sub-queries
  def apply [X:SqlType](column: String) = new SqlRawExpr[X](name+"."+column)
  def apply [X](col: ast.SqlNamedOptExpr[X]): ast.SqlNamedOptExpr[X] = new ast.SqlRawOptExpr[X](name+"."+col.name)(col.sqlTpe).as(col.name)
  def apply [X](col: ast.SqlNamedStrictExpr[X]): ast.SqlNamedStrictExpr[X] = new ast.SqlRawExpr[X](name+"."+col.name)(col.sqlTpe).as(col.name)
}

trait SqlNamedStrictExpr [T] extends SqlNamedExpr[T,T]
trait SqlNamedOptExpr [T] extends SqlNamedExpr[T,Option[T]] 

private [scoop] class SqlNamedExprImpl [T:SqlType] (val name: String, exprSql: String, val params: List[SqlParam[_]]) 
    extends SqlExpr[T] with SqlNamedStrictExpr[T] with Selection[T] {self =>
  val sqlTpe = SqlType[T]

  def sql = exprSql

  def as (alias: String): SqlNamedExpr[T,T] = new SqlNamedExprImpl [T] (alias, sql, params)

  def parse (rs: ResultSet): Option[T] = SqlType[T].parse(rs, name)
}

private [scoop] class SqlOptNamedExprImpl[T:SqlType] (val name: String, exprSql: String, val params: List[SqlParam[_]])
    extends SqlExpr[T] with SqlNamedOptExpr[T] with Selection[Option[T]] {self =>

  val sqlTpe = SqlType[T]

  def sql = exprSql

  def as (alias: String): SqlNamedExpr[T,Option[T]] = new SqlOptNamedExprImpl [T] (alias, sql, params)

  def parse (rs: ResultSet) = Some(SqlType[T].parse(rs, name))
}

/** Named query expression (returns one column) */
private [scoop] class SqlNamedQueryExpr[I:SqlType] (queryExpr: SqlQueryExpr[I], val name: String) extends SqlNamedExpr[I,I] {
 
  val sqlTpe = SqlType[I] 

  def sql = "(" + queryExpr.sql + ")"
  def params = queryExpr.params

  def parse (rs: ResultSet) = SqlType[I].parse(rs, name)
  
  def on (pred: SqlExpr[Boolean]) = query.Join(selectSql, pred.sql, params ++ pred.params)
}

/** Named query (untyped) */
private [scoop] class SqlNamedQuery (val query: Query, val name: String) extends SqlNamed {
  val sql = "(" + query.sql + ") as " + name
  val params = query.params

  // generate a column alias
  def apply [X:SqlType](column: String) = new SqlRawExpr[X](name+"."+column)
  def apply [X](col: ast.SqlNamedOptExpr[X]): ast.SqlNamedOptExpr[X] = new ast.SqlRawOptExpr[X](name+"."+col.name)(col.sqlTpe).as(col.name)
  def apply [X](col: ast.SqlNamedStrictExpr[X]): ast.SqlNamedStrictExpr[X] = new ast.SqlRawExpr[X](name+"."+col.name)(col.sqlTpe).as(col.name)

  def on (pred: SqlExpr[Boolean]) = Join(sql, pred.sql, params ++ pred.params)
}

