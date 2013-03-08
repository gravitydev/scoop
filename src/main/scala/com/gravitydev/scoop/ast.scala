package com.gravitydev.scoop
package ast

import java.sql.ResultSet

sealed trait Sql {
  def sql: String
}

sealed trait SqlExpr [X] extends Sql {self =>
  implicit def tp: SqlType[X]
  
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
    SqlInfixExpr[X,Set[X],Boolean](this, SqlLiteralSetExpr(v), "IN")
  }
  
  def notIn (v: Set[X])(implicit tp: SqlType[X]) = {
    SqlInfixExpr[X,Set[X],Boolean](this, SqlLiteralSetExpr(v), "NOT IN")
  }
 
  // should I get rid of these in favor of the symbolic ones?
  def and (v: SqlExpr[Boolean])(implicit ev: SqlExpr[X] =:= SqlExpr[Boolean]) = SqlInfixExpr[Boolean,Boolean,Boolean](ev(this), v, "AND")
  def or  (v: SqlExpr[Boolean])(implicit ev: SqlExpr[X] =:= SqlExpr[Boolean]) = SqlInfixExpr[Boolean,Boolean,Boolean](ev(this), v, "OR")
  
  // symbolic aliases for better precedence rules
  def && (v: SqlExpr[Boolean])(implicit ev: SqlExpr[X] =:= SqlExpr[Boolean]) = and(v)
  def ||  (v: SqlExpr[Boolean])(implicit ev: SqlExpr[X] =:= SqlExpr[Boolean]) = or(v)
  
  def isNull = SqlUnaryExpr[X,Boolean](this, "IS NULL", postfix=true)
  def isNotNull = SqlUnaryExpr[X,Boolean](this, "IS NOT NULL", postfix=true)

  def like (v: SqlLiteralExpr[String])(implicit ev: X =:= String) = SqlInfixExpr[X,String,Boolean](this, v, "LIKE")
  def notLike (v: SqlLiteralExpr[String])(implicit ev: X =:= String) = SqlInfixExpr[X,String,Boolean](this, v, "NOT LIKE")
  
  // TODO: decimals
  def + [T](v: SqlLiteralExpr[T])(implicit tp: SqlType[T], tp2: SqlType[X], ev: X => Long, ev2: T => Long) = SqlInfixExpr[X,T,Long](this, v, "+")
  def - [T](v: SqlLiteralExpr[T])(implicit tp: SqlType[T], tp2: SqlType[X], ev: X => Long, ev2: T => Long) = SqlInfixExpr[X,T,Long](this, v, "-")
  
  def as (alias: String)(implicit t: SqlType[X]) = new SqlNamedReqExpr[X] {
    val tp = t
    def name = alias
    def sql = self.sql
    def params = self.params
  }
}

abstract class BaseSqlExpr [T : SqlType] extends SqlExpr[T] {
  def tp = implicitly[SqlType[T]]
}

trait SqlNamedExpr [T] {
  implicit def tp: SqlType[T]
  def name: String
  def sql: String
  def params: Seq[SqlParam[_]]
}
trait SqlNamedReqExpr [T] extends SqlNamedExpr[T] {self =>
  def as (alias: String) = new BaseSqlExpr [T] with SqlNamedReqExpr [T] {
    def name = alias
    def sql = self.sql
    def params = self.params
  }
}
trait SqlNamedOptExpr [T] extends SqlNamedExpr[T] {self =>
  def as (alias: String) = new BaseSqlExpr [T] with SqlNamedOptExpr [T] {
    def name = alias
    def sql = self.sql
    def params = self.params
  }
}

/**
 * Typed query with one column
 */
case class SqlQueryExpr[T:SqlType] (query: com.gravitydev.scoop.query.Query) extends BaseSqlExpr[T] {
  def sql = "(" + query.sql + ")"
  def params = query.params
}

case class SqlRawExpr [X : SqlType] (sql: String, params: Seq[SqlParam[_]] = Nil) extends BaseSqlExpr[X]

abstract class SqlTable [T <: SqlTable[T]](_companion: TableCompanion[T], tableName: String = null, schema: String = null) {self: T =>
  // hmm... this is a bit brittle, but it *is* convenient
  val _tableName = Option(tableName) getOrElse _companion.getClass.getName.split('.').last.split('$').last

  val _schema = Option(schema)
  
  // Mutable for convenience
  // should only be changed by scoop
  private var __alias = _tableName
  
  def _alias = __alias
  def _prefix = _alias + "_" 

  implicit def _self = this
  def col[T](name: Symbol, cast: String = null)(implicit st: SqlType[T]) = new SqlNonNullableCol[T](name.name, Option(cast), this, st)
  
  def as (alias: String): T = {
    val t = _companion.apply
    t.__alias = alias
    t
  }
  
  // so it can serve as a companion
  def apply (): T = this
  
  def sql = if (_tableName == _alias) _tableName else _tableName + " as " + _alias
  def updateSql = _tableName
}

class SqlAssignment [T : SqlType](val col: SqlCol[T], value: SqlExpr[T]) extends BaseSqlExpr[Unit] {
  // assignments should have only the table name, never the alias
  def sql = col.columnName + " = " + valueSql
  def valueSql = value.sql + col.cast.map("::" + _).getOrElse("")
  override def params = value.params
}

case class SqlInfixExpr [L:SqlType, R:SqlType, T:SqlType](l: SqlExpr[L], r: SqlExpr[R], op: String) extends BaseSqlExpr[T] {
  def params = l.params ++ r.params
  def sql = "(" + l.sql + " " + op + " " + r.sql + ")"
  override def toString = "SqlInfixExpr(sql=" + sql + ", params=" + renderParams(params) + ")"
}

sealed abstract class SqlCol[T:SqlType] (val cast: Option[String], table: SqlTable[_], sqlType: SqlType[T], alias: String = null) extends BaseSqlExpr[T] {
  def columnName: String
  def name: String = Option(alias) getOrElse (table._prefix + columnName)
  val params = Nil
  def sql = table._alias + "." + columnName + cast.map(_=>"::varchar").getOrElse("") // TODO: use correct base type
  //def selectSql = sql + (if (table._prefix!="") " as " + _alias else "")
  //def as (s: String): SqlCol[T]
}

class SqlNonNullableCol[T:SqlType](val columnName: String, cast: Option[String], table: SqlTable[_], sqlType: SqlType[T], alias: String = null) 
    extends SqlCol[T] (cast, table, sqlType, alias) with SqlNamedReqExpr[T] {
  def parse (rs: ResultSet) = sqlType.parse(rs, name)
  override def toString = "Col(" + columnName + " as " + name + ")"
  def nullable = new SqlNullableCol(columnName, cast, table, sqlType)
  def := (x: SqlExpr[T]) = new SqlAssignment(this, x)
  //override def as (s: String): SqlNonNullableCol[T] = new SqlNonNullableCol[T](columnName, cast, table, sqlType, s)
}

class SqlNullableCol[T:SqlType](val columnName: String, cast: Option[String], table: SqlTable[_], sqlType: SqlType[T], alias: String = null) 
    extends SqlCol[T] (cast, table, sqlType, alias) with SqlNamedOptExpr[T] {
  def parse (rs: ResultSet) = Some(sqlType.parse(rs, name))
  override def toString = "Col("+ columnName + " as " + name +" : Nullable)"
  def := (x: Option[SqlExpr[T]]) = new SqlAssignment(this, x getOrElse new SqlRawExpr[T]("NULL"))
  //override def as (s: String) = new SqlNullableCol[T](columnName, cast, table, sqlType, s)
}

case class SqlUnaryExpr [L:SqlType,T:SqlType](l: SqlExpr[L], op: String, postfix: Boolean) extends BaseSqlExpr [T] {
  def params = l.params
  def sql = "(" + (if (postfix) l.sql + " " + op else op + " " + l.sql) + ")"
}

case class SqlLiteralExpr [T:SqlType] (v: T) extends BaseSqlExpr[T] {
  override def params = List(SqlSingleParam(v))
  def sql = "?"
}

case class SqlLiteralSetExpr [T:SqlType] (v: Set[T]) extends BaseSqlExpr[Set[T]] {
  override def params = SqlSetParam(v).toList
  def sql = v.toList.map(_ => "?").mkString("(", ", ", ")")
}

