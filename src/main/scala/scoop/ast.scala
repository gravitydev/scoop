package com.gravitydev.scoop 
package ast

import java.sql.{PreparedStatement, ResultSet}
import parsers.ResultSetParser
import query.SqlS

sealed trait Sql {
  def sql: String
}

trait SqlExpr [X] extends Sql {self =>
  implicit def sqlTpe: SqlType[X]
  
  def params: Seq[SqlParam[_]]
  
  def === (v: SqlExpr[X]) = SqlInfixExpr[Boolean](this, v, "=")

  def <> (v: SqlExpr[X]) = SqlInfixExpr[Boolean](this, v, "<>")
 
  // alias
  def |=| (v: SqlExpr[X]) = === (v)
  
  def <   (v: SqlExpr[X]) = SqlInfixExpr[Boolean](this, v, "<")
  def <=  (v: SqlExpr[X]) = SqlInfixExpr[Boolean](this, v, "<=")
  def >   (v: SqlExpr[X]) = SqlInfixExpr[Boolean](this, v, ">")
  def >=  (v: SqlExpr[X]) = SqlInfixExpr[Boolean](this, v, ">=")
  
  // it would be nice to have a view bound here
  def in (v: Set[X]) = if (v.isEmpty) SqlLiteralExpr(false) else SqlInfixExpr[Boolean](this, SqlLiteralSetExpr(v), "IN")
  
  def notIn (v: Set[X]) = if (v.isEmpty) SqlLiteralExpr(true) else SqlInfixExpr[Boolean](this, SqlLiteralSetExpr(v), "NOT IN")
 
  // should I get rid of these in favor of the symbolic ones?
  def and (v: SqlExpr[Boolean])(implicit ev: SqlExpr[X] =:= SqlExpr[Boolean]) = SqlInfixExpr[Boolean](ev(this), v, "AND")
  def or  (v: SqlExpr[Boolean])(implicit ev: SqlExpr[X] =:= SqlExpr[Boolean]) = SqlInfixExpr[Boolean](ev(this), v, "OR")
  
  // symbolic aliases for better precedence rules
  def && (v: SqlExpr[Boolean])(implicit ev: SqlExpr[X] =:= SqlExpr[Boolean]) = and(v)
  def ||  (v: SqlExpr[Boolean])(implicit ev: SqlExpr[X] =:= SqlExpr[Boolean]) = or(v)
  
  def isNull = SqlUnaryExpr[X,Boolean](this, "IS NULL", postfix=true)
  def isNotNull = SqlUnaryExpr[X,Boolean](this, "IS NOT NULL", postfix=true)

  def like (v: SqlExpr[String])(implicit ev: SqlType[X] with SqlUnderlyingType[String]) = SqlInfixExpr[Boolean](this, v, "LIKE")
  def notLike (v: SqlExpr[String])(implicit ev: SqlType[X] with SqlUnderlyingType[String]) = SqlInfixExpr[Boolean](this, v, "NOT LIKE")
 
  // TODO: decimals
  def + [T,N](v: SqlExpr[T])(implicit ev1: SqlExpr[X]=>SqlExpr[N], ev2: SqlExpr[T]=>SqlExpr[N], ev3: SqlType[N]) = SqlInfixExpr[N](this, v, "+")
  def - [T,N](v: SqlExpr[T])(implicit ev1: SqlExpr[X]=>SqlExpr[N], ev2: SqlExpr[T]=>SqlExpr[N], ev3: SqlType[N]) = SqlInfixExpr[N](this, v, "-")
  def * [T,N](v: SqlExpr[T])(implicit ev1: SqlExpr[X]=>SqlExpr[N], ev2: SqlExpr[T]=>SqlExpr[N], ev3: SqlType[N]) = SqlInfixExpr[N](this, v, "*")
  def / [T,N](v: SqlExpr[T])(implicit ev1: SqlExpr[X]=>SqlExpr[N], ev2: SqlExpr[T]=>SqlExpr[N], ev3: SqlType[N]) = SqlInfixExpr[N](this, v, "/")
 
  def desc  = new builder.SqlOrdering(this, builder.SqlOrder.Descending)
  def asc   = new builder.SqlOrdering(this, builder.SqlOrder.Ascending)

  type TypeMapper[A,B] = SqlType[A] => SqlType[B]
  type ExprMapper[A,B,C] = SqlExpr[A] => SqlExpr[B] => SqlExpr[C]
}

/** Expression that will produce an strict value when parsed out of a ResultSet */
trait SqlParseExpr[T] extends SqlExpr[T] {
  def as (alias: String): SqlNamedStrictExpr[T] = new SqlNamedExprImpl [T] (alias, sql, params)
}

/** Expression that will produce an optional value when parsed out of a ResultSet */
trait SqlOptParseExpr[T] extends SqlExpr[T] {
  def as (alias: String): SqlNamedOptExpr[T] = new SqlOptNamedExprImpl [T] (alias, sql, params)
}

abstract class SqlBaseExpr[T:SqlType] extends SqlParseExpr[T] {
  val sqlTpe = SqlType[T]
}

object SqlExpr {
  implicit def intToSqlLongLit (base: Int): SqlExpr[Long] = SqlLiteralExpr(base: Long)
}

class SqlWrappedExpr [T,X:SqlType] (sqlExpr: SqlExpr[T]) extends SqlBaseExpr[X] {
  def params = sqlExpr.params
  def sql = sqlExpr.sql
}

/**
 * Typed query with one column
 */
case class SqlQueryExpr[I:SqlType] (query: com.gravitydev.scoop.builder.Query, expr: SqlParseExpr[I]) extends SqlExpr[I] {
  val sqlTpe = SqlType[I]
  override def sql = "(" + util.formatSubExpr(query.rawQuery.sql) + ")"
  def params = query.rawQuery.params
  
  def as (alias: String) = new SqlNamedQueryExpr[I](this, alias)
}

private [scoop] class SqlRawParamExpr [X:SqlType] (val sql: String, val params: Seq[SqlParam[_]]) extends SqlBaseExpr[X]

class SqlRawExpr[X:SqlType] (val sql: String, val params: Seq[SqlParam[_]] = Nil) extends SqlBaseExpr[X]
class SqlRawOptExpr[X:SqlType] (val sql: String, val params: Seq[SqlParam[_]] = Nil) extends SqlOptParseExpr[X] {
  val sqlTpe = SqlType[X]
}

abstract class SqlTable [T <: SqlTable[T]](_companion: TableCompanion[T], tableName: String = null, schema: String = null) {self: T =>
  // hmm... this is a bit brittle, but it *is* convenient
  val _tableName: String = Option(tableName) getOrElse _companion.getClass.getName.split('.').last.split('$').last

  val _schema = Option(schema)
  
  // Mutable for convenience
  // should only be changed by scoop
  private var __alias = _tableName
  
  def _alias = __alias
  def _prefix = _alias + "_" 

  implicit def _self = this

  def col[T:SqlType](name: Symbol, cast: String = null) = new SqlNonNullableCol[T](name.name, Option(cast), this)
  
  def as (alias: String): T = {
    val t = _companion.apply
    t.__alias = alias
    t
  }
  
  // so it can serve as a companion
  def apply (): T = this

  // generate a column alias
  def apply [X:SqlType](column: String) = new SqlRawExpr[X](_alias+"."+column)
  
  def sql = if (_tableName == _alias) _tableName else _tableName + " as " + _alias
  def updateSql = _tableName
}

class SqlAssignment [T:SqlType](val col: SqlCol[T], value: SqlExpr[T]) extends SqlExpr[Unit] {
  // hack
  val sqlTpe = null

  // assignments should have only the table name, never the alias
  def sql = col.columnName + " = " + valueSql
  def valueSql = value.sql + col.cast.map("::" + _).getOrElse("")
  override def params = value.params
}

case class SqlInfixExpr [T:SqlType](l: SqlExpr[_], r: SqlExpr[_], op: String) extends SqlBaseExpr[T] {
  def params = l.params ++ r.params
  def sql = "(" + l.sql + " " + op + " " + r.sql + ")"
  override def toString = "SqlInfixExpr(sql=" + sql + ", params=" + renderParams(params) + ")"
}

/**
 * @param name Name of the column to parse out of the ResultSet (might be an alias)
 */
sealed abstract class SqlCol[T:SqlType] (val cast: Option[String], table: SqlTable[_], explicitAlias: String = null) extends SqlExpr[T] {
  val sqlTpe = SqlType[T]

  def columnName: String

  def name: String = Option(explicitAlias) getOrElse table._prefix + columnName

  val params = Nil

  def sql = table._alias + "." + columnName + cast.map(_=>"::varchar").getOrElse("") // TODO: use correct base type

  def expressions: List[builder.SelectExpr] // = List( new query.SelectExprS(sql + " as " + name) )
}

class SqlNonNullableCol[T:SqlType](val columnName: String, cast: Option[String], table: SqlTable[_], explicitAlias: String = null) 
    extends SqlCol[T] (cast, table, explicitAlias) with SqlParseExpr[T] with SqlNamedStrictExpr[T] with Selection[T] {

  override def toString = "Col(" + columnName + " as " + name + ")"

  def parse (rs: ResultSet): Option[T] = SqlType[T].parse(rs, name)

  def nullable = new SqlNullableCol[T](columnName, cast, table, explicitAlias)
  def := (x: SqlExpr[T]) = new ast.SqlAssignment(this, x)
}

class SqlNullableCol[T:SqlType](val columnName: String, cast: Option[String], table: SqlTable[_], explicitAlias: String) 
    extends SqlCol[T] (cast, table, explicitAlias) with SqlOptParseExpr[T] with SqlNamedOptExpr[T] with Selection[Option[T]] {

  override def toString = "Col("+ columnName + " as " + name +" : Nullable)"

  def parse (rs: ResultSet): Option[Option[T]] = Some(SqlType[T].parse(rs, name))

  def := (x: Option[SqlExpr[T]]) = new SqlAssignment(this, x getOrElse new SqlRawExpr[T]("NULL"))
}

case class SqlUnaryExpr [L:SqlType,T:SqlType](l: SqlExpr[L], op: String, postfix: Boolean) extends SqlBaseExpr [T] {
  def params = l.params
  def sql = "(" + (if (postfix) l.sql + " " + op else op + " " + l.sql) + ")"
}

case class SqlLiteralExpr [T:SqlType] (v: T) extends SqlBaseExpr[T] {
  override def params = List(SqlSingleParam(v))
  def sql = "?"
}

case class SqlLiteralSetExpr [T:SqlType] (v: Set[T]) extends SqlBaseExpr[Set[T]] {
  override def params = SqlSetParam(v).toList
  def sql = v.toList.map(_ => "?").mkString("(", ", ", ")")
}

