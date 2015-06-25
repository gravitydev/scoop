package com.gravitydev.scoop
package ast

import query.{stringToFragment, ParameterizedSql}
import java.sql.ResultSet

/** 
 * Untyped named sql 
 * Useful for select clause, from clause, etc, but not for parsing since we don't know the type (subtype might)
 */
trait SqlNamed {
  def name: String
  //def sql: ParameterizedSql
}

/** Typed parseable sql expression */
trait SqlNamedExpr [I,+O] extends SqlExpr[I] with SqlNamed with parsers.ExprSelection[O] {self =>
  def parse (rs: ResultSet): Option[O]

  def apply (rs: ResultSet): ParseResult[O] = parse(rs).toRight("Column ["+name+"] not found in ResultSet: "+util.inspectRS(rs))

  def name: String

  def expressions = List(this)

  // this should only be applicable to sub-queries
  def apply [X:SqlType](column: String) = new SqlRawExpr[X](name+"."+column)
  def apply [X](col: ast.SqlNamedOptExpr[X]): ast.SqlNamedOptExpr[X] = new ast.SqlRawOptExpr[X](name+"."+col.name)(col.sqlTpe).as(col.name)
  def apply [X](col: ast.SqlNamedStrictExpr[X]): ast.SqlNamedStrictExpr[X] = new ast.SqlRawExpr[X](name+"."+col.name)(col.sqlTpe).as(col.name)
}

private [scoop] case class SqlNamedStrictExpr [T:SqlType] (expr: SqlExpr[T], name: String) 
    extends SqlNamedExpr[T,T] with Selection[T] {self =>
  val sqlTpe = SqlType[T]

  def as (alias: String): SqlNamedExpr[T,T] = SqlNamedStrictExpr[T](expr, alias)

  def parse (rs: ResultSet): Option[T] = SqlType[T].parse(rs, name)

  override def toString = "SqlNamedStrictExpr(" + expr + " as " + name + ")"
}

private [scoop] case class SqlNamedOptExpr[T:SqlType] (expr: SqlExpr[T], name: String)
    extends SqlNamedExpr[T,Option[T]] with Selection[Option[T]] {self =>

  val sqlTpe = SqlType[T]

  def as (alias: String): SqlNamedExpr[T,Option[T]] = SqlNamedOptExpr[T](expr, alias)

  def parse (rs: ResultSet) = Some(SqlType[T].parse(rs, name))
}

