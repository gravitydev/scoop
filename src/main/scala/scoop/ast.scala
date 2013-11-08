package com.gravitydev.scoop
package ast

import java.sql.{PreparedStatement, ResultSet}
import parsers.{ResultSetParser, ExprParser}

trait SqlType[T]

trait SqlParamType[T] extends SqlType[T] {
  def set (stmt: PreparedStatement, idx: Int, value: T): Unit
}

trait SqlResultType[T] extends SqlType[T] {
  def parse (rs: ResultSet, name: String): Option[T]
}

trait SqlMappedType [T] extends SqlParamType[T] with SqlResultType[T] {self =>
  def tpe: Int // jdbc sql type
  def apply (n: String, sql: query.SqlFragmentS = "") = new ExprParser (n, List(sql))(this)
}
  
sealed trait Sql {
  def sql: String
}

sealed trait SqlExpr [X] extends Sql {self =>
  implicit def paramTpe: SqlParamType[X]
  
  def params: Seq[SqlParam[_]]
  
  def === [Z](v: SqlExpr[Z]) = SqlInfixExpr[Boolean](this, v, "=")

  def <> [Z](v: SqlExpr[Z]) = SqlInfixExpr[Boolean](this, v, "<>")
 
  // alias
  def |=| (v: SqlExpr[X]) = === (v)
  
  def <   (v: SqlExpr[X]) = SqlInfixExpr[Boolean](this, v, "<")
  def <=  (v: SqlExpr[X]) = SqlInfixExpr[Boolean](this, v, "<=")
  def >   (v: SqlExpr[X]) = SqlInfixExpr[Boolean](this, v, ">")
  def >=  (v: SqlExpr[X]) = SqlInfixExpr[Boolean](this, v, ">=")
  
  // it would be nice to have a view bound here
  def in (v: Set[X]) = SqlInfixExpr[Boolean](this, SqlLiteralSetExpr(v), "IN")
  
  def notIn (v: Set[X]) = SqlInfixExpr[Boolean](this, SqlLiteralSetExpr(v), "NOT IN")
 
  // should I get rid of these in favor of the symbolic ones?
  def and (v: SqlExpr[Boolean])(implicit ev: SqlExpr[X] =:= SqlExpr[Boolean]) = SqlInfixExpr[Boolean](ev(this), v, "AND")
  def or  (v: SqlExpr[Boolean])(implicit ev: SqlExpr[X] =:= SqlExpr[Boolean]) = SqlInfixExpr[Boolean](ev(this), v, "OR")
  
  // symbolic aliases for better precedence rules
  def && (v: SqlExpr[Boolean])(implicit ev: SqlExpr[X] =:= SqlExpr[Boolean]) = and(v)
  def ||  (v: SqlExpr[Boolean])(implicit ev: SqlExpr[X] =:= SqlExpr[Boolean]) = or(v)
  
  def isNull = SqlUnaryExpr[X,Boolean](this, "IS NULL", postfix=true)
  def isNotNull = SqlUnaryExpr[X,Boolean](this, "IS NOT NULL", postfix=true)

  def like (v: SqlExpr[String])(implicit ev: X =:= String) = SqlInfixExpr[Boolean](this, v, "LIKE")
  def notLike (v: SqlExpr[String])(implicit ev: X =:= String) = SqlInfixExpr[Boolean](this, v, "NOT LIKE")
 
  // TODO: decimals
  def + [T,N](v: SqlExpr[T])(implicit ev1: SqlExpr[X]=>SqlExpr[N], ev2: SqlExpr[T]=>SqlExpr[N], ev3: SqlParamType[N]) = SqlInfixExpr[N](this, v, "+")
  def - [T,N](v: SqlExpr[T])(implicit ev1: SqlExpr[X]=>SqlExpr[N], ev2: SqlExpr[T]=>SqlExpr[N], ev3: SqlParamType[N]) = SqlInfixExpr[N](this, v, "-")
  def * [T,N](v: SqlExpr[T])(implicit ev1: SqlExpr[X]=>SqlExpr[N], ev2: SqlExpr[T]=>SqlExpr[N], ev3: SqlParamType[N]) = SqlInfixExpr[N](this, v, "*")
  def / [T,N](v: SqlExpr[T])(implicit ev1: SqlExpr[X]=>SqlExpr[N], ev2: SqlExpr[T]=>SqlExpr[N], ev3: SqlParamType[N]) = SqlInfixExpr[N](this, v, "/")
  
  def as (alias: String)(implicit ev: SqlResultType[X]) = new SqlNamedReqExpr[X] (
    name  = alias,
    sql   = sql + " as " + alias,
    params = params
  )

  def desc  = SqlOrdering(this, Descending)
  def asc   = SqlOrdering(this, Ascending)

  type TypeMapper[A,B] = SqlType[A] => SqlType[B]
  type ExprMapper[A,B,C] = SqlExpr[A] => SqlExpr[B] => SqlExpr[C]
}

abstract class BaseSqlExpr [T:SqlParamType] extends SqlExpr[T] {
  def paramTpe = implicitly[SqlParamType[T]]
}

class SqlNativeType[T] (val tpe: Int, get: (ResultSet, String) => T, _set: (PreparedStatement, Int, T) => Unit) extends SqlMappedType [T] {
  def set (stmt: PreparedStatement, idx: Int, value: T): Unit = {
    if (value==null) stmt.setNull(idx, tpe)
    else _set(stmt, idx, value)
  }
  def parse (rs: ResultSet, name: String) = Option(get(rs, name)) filter {_ => !rs.wasNull}
}
class SqlCustomType[T,N] (from: N => T, to: T => N)(implicit nt: SqlNativeType[N]) extends SqlMappedType[T] {
  def tpe = nt.tpe
  def parse (rs: ResultSet, name: String) = nt.parse(rs, name) map from
  def set (stmt: PreparedStatement, idx: Int, value: T): Unit = nt.set(stmt, idx, to(value))
}

object SqlExpr {
  implicit def intToSqlLongLit (base: Int): SqlExpr[Long] = SqlLiteralExpr(base: Long)
}

class SqlWrappedExpr [T,X:SqlParamType] (sqlExpr: SqlExpr[T]) extends BaseSqlExpr[X] {
  def params = sqlExpr.params
  def sql = sqlExpr.sql
}

abstract class SqlOrder (val sql: String)
case object Ascending   extends SqlOrder ("ASC")
case object Descending  extends SqlOrder ("DESC")
case class SqlOrdering (expr: ast.SqlExpr[_], order: SqlOrder) {
  def sql = expr.sql + " " + order.sql
  def params = expr.params
}

sealed trait SqlNamedExpr [T] extends SqlExpr[T] {self =>
  def resultTpe: SqlResultType[T]

  def name: String
  def sql: String
  def params: Seq[SqlParam[_]]

  // this should only be applicable to sub-queries
  def apply [X:SqlMappedType](column: String) = new SqlRawExpr[X](name+"."+column)
  def apply [X:SqlMappedType](col: SqlNonNullableCol[X]) = new SqlNamedReqExpr[X](
    name    = col.name,
    sql     = self.name+"."+col.name,
    params  = col.params
  )
  def apply [X:SqlMappedType](col: SqlNullableCol[X]) = new SqlNamedOptExpr[X](
    name  = col.name,
    sql   = self.name+"."+col.name,
    params = col.params
  )
  def apply [X:SqlMappedType](col: SqlNamedExpr[X]) = new SqlRawExpr[X](name+"."+col.name).as(col.name)

  def on (pred: SqlExpr[Boolean]) = query.Join(sql, pred.sql, params ++ pred.params)
}

object SqlNamedExpr {
  implicit def fromCol [T](c: SqlNonNullableCol[T]) = new SqlNamedReqExpr[T] (
    name    = c.name, 
    sql     = c.sql + " as " + c.name,
    params  = c.params
  )(c.paramTpe, c.resultTpe)
}

class SqlNamedReqExpr [T:SqlParamType:SqlResultType] (val name: String, val sql: String, val params: Seq[SqlParam[_]]) extends BaseSqlExpr[T] with SqlNamedExpr[T] {self =>
  val resultTpe = implicitly[SqlResultType[T]]
  def as (alias: String) = new SqlNamedReqExpr [T] (alias, sql, params)
  def parse (rs: ResultSet) = implicitly[SqlResultType[T]].parse(rs, name)
}

class SqlNamedOptExpr [T:SqlParamType:SqlResultType] (val name: String, val sql: String, val params: Seq[SqlParam[_]]) extends BaseSqlExpr[T] with SqlNamedExpr[T] {self =>
  val resultTpe = implicitly[SqlResultType[T]]
  def as (alias: String) = new SqlNamedOptExpr [T] (alias, sql, params)
  def parse (rs: ResultSet) = Some(resultTpe.parse(rs, name))
}

/**
 * Typed query with one column
 */
case class SqlQueryExpr[T:SqlParamType] (query: com.gravitydev.scoop.query.Query) extends BaseSqlExpr[T] {
  override def sql = "(" + util.formatSubExpr(query.sql) + ")"
  def params = query.params
  override def as (name: String)(implicit t: SqlResultType[T]) = new SqlNamedQueryExpr[T](this, name)
}

class SqlNamedQueryExpr[T:SqlParamType:SqlResultType] (queryExpr: SqlQueryExpr[T], name: String) 
  extends SqlNamedReqExpr[T](name, queryExpr.sql + " as " + name, queryExpr.params)

case class SqlRawExpr [X:SqlParamType] (sql: String, params: Seq[SqlParam[_]] = Nil) extends BaseSqlExpr[X]

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

  def col[T:SqlMappedType](name: Symbol, cast: String = null) = new SqlNonNullableCol[T](name.name, Option(cast), this)
  
  def as (alias: String): T = {
    val t = _companion.apply
    t.__alias = alias
    t
  }
  
  // so it can serve as a companion
  def apply (): T = this

  // generate a column alias
  def apply [X:SqlParamType](column: String) = new SqlRawExpr[X](_alias+"."+column)
  
  def sql = if (_tableName == _alias) _tableName else _tableName + " as " + _alias
  def updateSql = _tableName
}

class SqlAssignment [T:SqlParamType](val col: SqlCol[T], value: SqlExpr[T]) extends SqlExpr[Unit] {
  // hack
  def paramTpe = null
  // assignments should have only the table name, never the alias
  def sql = col.columnName + " = " + valueSql
  def valueSql = value.sql + col.cast.map("::" + _).getOrElse("")
  override def params = value.params
}

case class SqlInfixExpr [T:SqlParamType](l: SqlExpr[_], r: SqlExpr[_], op: String) extends BaseSqlExpr[T] {
  def params = l.params ++ r.params
  def sql = "(" + l.sql + " " + op + " " + r.sql + ")"
  override def toString = "SqlInfixExpr(sql=" + sql + ", params=" + renderParams(params) + ")"
}

sealed abstract class SqlCol[T:SqlParamType:SqlResultType] (val cast: Option[String], table: SqlTable[_], alias: String = null) extends BaseSqlExpr[T] {
  def columnName: String
  def name: String = Option(alias) getOrElse (table._prefix + columnName)
  val params = Nil
  def sql = table._alias + "." + columnName + cast.map(_=>"::varchar").getOrElse("") // TODO: use correct base type
}

class SqlNonNullableCol[T:SqlMappedType](val columnName: String, cast: Option[String], table: SqlTable[_], alias: String = null) 
    extends SqlCol[T] (cast, table, alias) {
  val resultTpe = implicitly[SqlResultType[T]]
  override def toString = "Col(" + columnName + " as " + name + ")"
  def nullable = new SqlNullableCol[T](columnName, cast, table)
  def := (x: SqlExpr[T]) = new SqlAssignment(this, x)
}

class SqlNullableCol[T:SqlMappedType](val columnName: String, cast: Option[String], table: SqlTable[_], alias: String = null) 
    extends SqlCol[T] (cast, table, alias) {
  override def toString = "Col("+ columnName + " as " + name +" : Nullable)"
  def := (x: Option[SqlExpr[T]]) = new SqlAssignment(this, x getOrElse new SqlRawExpr[T]("NULL"))
}

case class SqlUnaryExpr [L:SqlParamType,T:SqlParamType](l: SqlExpr[L], op: String, postfix: Boolean) extends BaseSqlExpr [T] {
  def params = l.params
  def sql = "(" + (if (postfix) l.sql + " " + op else op + " " + l.sql) + ")"
}

case class SqlLiteralExpr [T:SqlParamType] (v: T) extends BaseSqlExpr[T] {
  override def params = List(SqlSingleParam(v))
  def sql = "?"
}

case class SqlLiteralSetExpr [T:SqlParamType] (v: Set[T]) extends BaseSqlExpr[Set[T]] {
  override def params = SqlSetParam(v).toList
  def sql = v.toList.map(_ => "?").mkString("(", ", ", ")")
}

