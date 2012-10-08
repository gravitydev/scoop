package com.gravitydev.scoop
package ast

import java.sql.ResultSet

sealed trait Sql {
  def sql: String
}

sealed trait SqlExpr [X] extends Sql {
  def === [T <% X] (v: SqlExpr[T]) = SqlInfixExpr[X,T,Boolean](this, v, "=")
  // alias
  def |=| [T <% X] (v: SqlExpr[T]) = === (v)
  
  def <   [T <% X] (v: SqlExpr[T]) = SqlInfixExpr[X,T,Boolean](this, v, "<")
  def <=  [T <% X] (v: SqlExpr[T]) = SqlInfixExpr[X,T,Boolean](this, v, "<=")
  def >   [T <% X] (v: SqlExpr[T]) = SqlInfixExpr[X,T,Boolean](this, v, ">")
  def >=  [T <% X] (v: SqlExpr[T]) = SqlInfixExpr[X,T,Boolean](this, v, ">=")
  
  def and [V](v: SqlExpr[V]) = SqlInfixExpr[X,V,Boolean](this, v, "AND")
  def or  [V](v: SqlExpr[V]) = SqlInfixExpr[X,V,Boolean](this, v, "OR")
  
  def isNull = SqlUnaryPostfixExpr[X,Boolean](this, "IS NULL")
  def isNotNull = SqlUnaryPostfixExpr[X,Boolean](this, "IS NOT NULL")
  
  def like (v: SqlLiteralExpr[String])(implicit ev: X =:= String) = SqlInfixExpr[X,String,Boolean](this, v, "LIKE")
  def notLike (v: SqlLiteralExpr[String])(implicit ev: X =:= String) = SqlInfixExpr[X,String,Boolean](this, v, "NOT LIKE")
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
  def col[T](name: String)(implicit st: SqlType[T,_]) = new SqlCol[T](name)
  def col[T](name: Symbol)(implicit st: SqlType[T,_]) = new SqlCol[T](name.name)
  def as (alias: String): T = companion(alias)
  def sql = tableName + " as " + as
}

class SqlCol[T](val name: String)(implicit val table: SqlTable[_], sqlType: SqlType[T,_]) extends SqlExpr[T] {
  def parse (rs: ResultSet, alias: String = null) = sqlType.get(Option(alias) getOrElse name)(rs)
  override def toString = "Col("+name+")"
  def sql = table.as + "." + name
}

case class SqlUnaryPostfixExpr [L,T](l: SqlExpr[L], op: String) extends SqlExpr [T] {
  def sql = "(" + l.sql + " " + op + ")"
}

case class SqlInfixExpr [L,R,T](l: SqlExpr[L], r: SqlExpr[R], op: String) extends SqlExpr[T] {
  def sql = "(" + l.sql + " " + op + " " + r.sql + ")"
}

case class SqlLiteralExpr [T] (v: T) extends SqlExpr[T] {
  def sql = v.toString
}
