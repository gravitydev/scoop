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
  def pf: String
  implicit def _self = this
  def col[T](name: String, cast: String = null)(implicit st: SqlType[T]) = new SqlNonNullableCol[T](name, Option(cast))
  def col[T](name: Symbol)(implicit st: SqlType[T]) = new SqlNonNullableCol[T](name.name, None)
  def as (alias: String): T = companion(alias, pf)
  
  // prefix to use for columns
  def prefix (_pf: String): T = companion(as, _pf)
  
  def sql = tableName + " as " + as
}

class SqlAssignment [T](col: SqlCol[T], value: T)(implicit sqlType: SqlType[T]) extends SqlExpr[Unit] {
  def sql = col.name + " = ?"
  override def params = List(SqlSingleParam(value))
}

sealed abstract class SqlCol[T] (cast: Option[String])(implicit val table: SqlTable[_], sqlType: SqlType[T]) extends SqlExpr[T] {
  def name: String
  def alias = table.pf + name
  val params = Nil
  def sql = table.as + "." + name + cast.map("::"+_).getOrElse("")
  def selectSql = sql + cast.map("::"+_).getOrElse("") + (if (table.pf!="") " as " + alias else "")
}

class SqlNonNullableCol[T](val name: String, val cast: Option[String])(implicit table: SqlTable[_], sqlType: SqlType[T]) extends SqlCol[T] (cast) {
  def parse (rs: ResultSet) = sqlType.parse(rs, alias)
  override def toString = "Col(" + selectSql + ")"
  def nullable = new SqlNullableCol(name, cast)(table, sqlType)
  def := (x: T) = new SqlAssignment(this, x)
}

class SqlNullableCol[T](val name: String, val cast: Option[String])(implicit table: SqlTable[_], sqlType: SqlType[T]) extends SqlCol[T] (cast) {
  def parse (rs: ResultSet) = Some(sqlType.parse(rs, alias))
  override def toString = "NullableCol("+selectSql+")"
  def := (x: Option[T]) = new SqlAssignment(this, x getOrElse null.asInstanceOf[T]) // TODO: yes, this is a hack
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
