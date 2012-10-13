package com.gravitydev.scoop
package ast

import java.sql.ResultSet

sealed trait Sql {
  def sql: String
}

sealed trait SqlExpr [X] extends Sql {
  def params: List[SqlSingleParam[_,_]]
  
  def === [T <% X] (v: SqlExpr[T]) = SqlInfixExpr[X,T,Boolean](this, v, "=")
  
  // alias
  def |=| [T <% X] (v: SqlExpr[T]) = === (v)
  
  def <   [T <% X] (v: SqlExpr[T]) = SqlInfixExpr[X,T,Boolean](this, v, "<")
  def <=  [T <% X] (v: SqlExpr[T]) = SqlInfixExpr[X,T,Boolean](this, v, "<=")
  def >   [T <% X] (v: SqlExpr[T]) = SqlInfixExpr[X,T,Boolean](this, v, ">")
  def >=  [T <% X] (v: SqlExpr[T]) = SqlInfixExpr[X,T,Boolean](this, v, ">=")
  
  // it would be nice to have a view bound here
  def in [T >: X] (v: Set[T])(implicit tp: SqlType[T]) = {
    SqlInfixExpr[X,Set[T],Boolean](this, SqlLiteralSetExpr(v), "in")
  }
  
  def and [V](v: SqlExpr[V]) = SqlInfixExpr[X,V,Boolean](this, v, "AND")
  def or  [V](v: SqlExpr[V]) = SqlInfixExpr[X,V,Boolean](this, v, "OR")
  
  def isNull = SqlUnaryPostfixExpr[X,Boolean](this, "IS NULL")
  def isNotNull = SqlUnaryPostfixExpr[X,Boolean](this, "IS NOT NULL")
  
  def like [V >: X](v: SqlLiteralExpr[String])(implicit ev: V =:= String) = SqlInfixExpr[X,String,Boolean](this, v, "LIKE")
  def notLike [V >: X](v: SqlLiteralExpr[String])(implicit ev: V =:= String) = SqlInfixExpr[X,String,Boolean](this, v, "NOT LIKE")
}

trait Queryable[T] {
  def as (alias: String): T
  def as: String
  def sql: String
}

abstract class SqlTable [T <: SqlTable[T]](_tableName: String, companion: TableCompanion[T]) extends Queryable[T] {
  val tableName = Option(_tableName) getOrElse companion.getClass.getCanonicalName.split('.').last.stripSuffix("$")
  def as: String
  implicit def _self = this
  def col[T](name: String)(implicit st: SqlType[T]) = new SqlCol[T](name)
  def col[T](name: Symbol)(implicit st: SqlType[T]) = new SqlCol[T](name.name)
  def as (alias: String): T = companion(alias)
  def sql = tableName + " as " + as
}

class SqlCol[T](val name: String)(implicit val table: SqlTable[_], sqlType: SqlType[T]) extends SqlExpr[T] {
  val params = Nil
  def parse (rs: ResultSet, alias: String = null) = sqlType.parse(rs, Option(alias) getOrElse name)
  override def toString = "Col("+name+")"
  def sql = table.as + "." + name
  
  def nullable = new SqlNullableCol(this)
}

class SqlNullableCol[T](col: SqlCol[T]) extends SqlExpr[T] {
  val name = col.name
  def params = col.params
  def sql = col.sql
  def parse (rs: ResultSet, alias: String = null) = Some(col.parse(rs, alias))
  override def toString = "NullableCol("+col.name+")"
}

case class SqlUnaryPostfixExpr [L,T](l: SqlExpr[L], op: String) extends SqlExpr [T] {
  def params = l.params
  def sql = "(" + l.sql + " " + op + ")"
}

case class SqlInfixExpr [L,R,T](l: SqlExpr[L], r: SqlExpr[R], op: String) extends SqlExpr[T] {
  def params = l.params ++ r.params
  def sql = "(" + l.sql + " " + op + " " + r.sql + ")"
  override def toString = "SqlInfixExpr(sql=" + sql + ", params=" + renderParams(params) + ")"
}

case class SqlLiteralExpr [T] (v: T)(implicit tp: SqlType[T]) extends SqlExpr[T] {
  override def params = List(SqlSingleParam(v))
  def sql = "?"
}

case class SqlLiteralSetExpr [T] (v: Set[T])(implicit tp: SqlType[T]) extends SqlExpr[Set[T]] {
  override def params = SqlSetParam(v).toList
  def sql = v.toList.map(_ => "?").mkString("(", ", ", ")")
}
