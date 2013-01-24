package com.gravitydev.scoop
package ast

import java.sql.ResultSet

sealed trait Sql {
  def sql: String
}

sealed trait SqlExpr [X] extends Sql {
  def params: Seq[SqlParam[_]]
  
  def === (v: SqlExpr[X]) = SqlInfixExpr[X,X,Boolean](this, v, "=")

  def <> (v: SqlExpr[X]) = SqlInfixExpr[X,X,Boolean](this, v, "<>")
  
  // alias
  def |=| (v: SqlExpr[X]) = === (v)
  
  def <   (v: SqlExpr[X]) = SqlInfixExpr[X,X,Boolean](this, v, "<")
  def <=  (v: SqlExpr[X]) = SqlInfixExpr[X,X,Boolean](this, v, "<=")
  def >   (v: SqlExpr[X]) = SqlInfixExpr[X,X,Boolean](this, v, ">")
  def >=  (v: SqlExpr[X]) = SqlInfixExpr[X,X,Boolean](this, v, ">=")
  
  // it would be nice to have a view bound here
  def in (v: Set[X])(implicit tp: SqlType[X]) = { // is the implicit needed here
    SqlInfixExpr[X,Set[X],Boolean](this, SqlLiteralSetExpr(v), "in")
  }
  
  def and (v: SqlExpr[Boolean])(implicit ev: SqlExpr[X] =:= SqlExpr[Boolean]) = SqlInfixExpr[Boolean,Boolean,Boolean](ev(this), v, "AND")
  def or  (v: SqlExpr[Boolean])(implicit ev: SqlExpr[X] =:= SqlExpr[Boolean]) = SqlInfixExpr[Boolean,Boolean,Boolean](ev(this), v, "OR")
  
  // symbolic aliases for better precedence rules
  def && (v: SqlExpr[Boolean])(implicit ev: SqlExpr[X] =:= SqlExpr[Boolean]) = and(v)
  def ||  (v: SqlExpr[Boolean])(implicit ev: SqlExpr[X] =:= SqlExpr[Boolean]) = or(v)
  
  def isNull = SqlUnaryPostfixExpr[X,Boolean](this, "IS NULL")
  def isNotNull = SqlUnaryPostfixExpr[X,Boolean](this, "IS NOT NULL")
  
  def like (v: SqlLiteralExpr[String])(implicit ev: X =:= String) = SqlInfixExpr[X,String,Boolean](this, v, "LIKE")
  def notLike (v: SqlLiteralExpr[String])(implicit ev: X =:= String) = SqlInfixExpr[X,String,Boolean](this, v, "NOT LIKE")
  
  // TODO: decimals
  def + [T](v: SqlLiteralExpr[T])(implicit ev: X => Long, ev2: T => Long) = SqlInfixExpr[X,T,Long](this, v, "+")
  def - [T](v: SqlLiteralExpr[T])(implicit ev: X => Long, ev2: T => Long) = SqlInfixExpr[X,T,Long](this, v, "-")
  
  // TODO: make this work
  //def as (alias: String) = new query.SelectExprS(this.sql)
}

case class SqlRawExpr [X] (sql: String, params: Seq[SqlParam[_]] = Nil) extends SqlExpr[X]

abstract class SqlTable [T <: SqlTable[T]](_companion: TableCompanion[T], tableName: String = null, schema: String = null) {self: T =>
  val _tableName = Option(tableName) getOrElse _companion.getClass.getCanonicalName.split('.').last.split('$').last
  val _schema = Option(schema)
  
  // Mutable for convenience
  // should only be changed by scoop
  private var __alias = _tableName
  
  def _alias = __alias
  def _prefix = _alias + "_" 

  implicit def _self = this
  def col[T](name: String, cast: String = null)(implicit st: SqlType[T]) = new SqlNonNullableCol[T](name, Option(cast), this, st)
  def col[T](name: Symbol)(implicit st: SqlType[T]) = new SqlNonNullableCol[T](name.name, None, this, st)
  
  def as (alias: String): T = {
    val t = _companion.apply
    t.__alias = alias
    t
  }
  
  // so it can serve as a companion
  def apply (): T = this
  
  def sql = _tableName + " as " + _alias
  def updateSql = _tableName
}

/*
sealed trait SqlAssignment extends SqlExpr[Unit]
class SqlLiteralAssignment [T](col: SqlCol[T], value: T)(implicit sqlType: SqlType[T]) extends SqlAssignment {
  def sql = col.name + " = ?"
  override def params = List(SqlSingleParam(value))
}
*/
class SqlAssignment [T](val col: SqlCol[T], value: SqlExpr[T]) extends SqlExpr[Unit] {
  def sql = col.name + " = " + valueSql
  def valueSql = value.sql + col.cast.map("::" + _).getOrElse("")
  override def params = value.params
}

case class SqlInfixExpr [L,R,T](l: SqlExpr[L], r: SqlExpr[R], op: String) extends SqlExpr[T] {
  def params = l.params ++ r.params
  def sql = "(" + l.sql + " " + op + " " + r.sql + ")"
  override def toString = "SqlInfixExpr(sql=" + sql + ", params=" + renderParams(params) + ")"
}

sealed abstract class SqlCol[T] (val cast: Option[String], table: SqlTable[_], sqlType: SqlType[T], alias: String = null) extends SqlExpr[T] {
  def name: String
  def _alias = Option(alias) getOrElse (table._prefix + name)
  val params = Nil
  def sql = table._alias + "." + name + cast.map(_=>"::varchar").getOrElse("") // TODO: use correct base type
  def selectSql = sql + (if (table._prefix!="") " as " + _alias else "")
  def as (s: String): SqlCol[T]
}

class SqlNonNullableCol[T](val name: String, cast: Option[String], table: SqlTable[_], sqlType: SqlType[T], alias: String = null) extends SqlCol[T] (cast, table, sqlType, alias) {
  def parse (rs: ResultSet) = sqlType.parse(rs, _alias)
  override def toString = "Col(" + selectSql + ")"
  def nullable = new SqlNullableCol(name, cast, table, sqlType)
  def := (x: SqlExpr[T]) = new SqlAssignment(this, x)//(sqlType)
  def as (s: String): SqlNonNullableCol[T] = new SqlNonNullableCol[T](name, cast, table, sqlType, s)
}

class SqlNullableCol[T](val name: String, cast: Option[String], table: SqlTable[_], sqlType: SqlType[T], alias: String = null) extends SqlCol[T] (cast, table, sqlType, alias) {
  def parse (rs: ResultSet) = Some(sqlType.parse(rs, _alias))
  override def toString = "NullableCol("+selectSql+")"
  def := (x: Option[SqlExpr[T]]) = new SqlAssignment(this, x getOrElse new SqlRawExpr[T]("NULL"))
  def as (s: String) = new SqlNullableCol[T](name, cast, table, sqlType, s)
}

case class SqlUnaryPostfixExpr [L,T](l: SqlExpr[L], op: String) extends SqlExpr [T] {
  def params = l.params
  def sql = "(" + l.sql + " " + op + ")"
}

case class SqlLiteralExpr [T] (v: T)(implicit tp: SqlType[T]) extends SqlExpr[T] {
  override def params = List(SqlSingleParam(v))
  def sql = "?"
}

case class SqlLiteralSetExpr [T] (v: Set[T])(implicit tp: SqlType[T]) extends SqlExpr[Set[T]] {
  override def params = SqlSetParam(v).toList
  def sql = v.toList.map(_ => "?").mkString("(", ", ", ")")
}

