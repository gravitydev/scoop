package com.gravitydev.scoop
package ast

import java.sql.{PreparedStatement, ResultSet}
import parsers.ResultSetParser
import query.SqlS

trait SqlType[T]

trait SqlParamType[T] extends SqlType[T] {
  def set (stmt: PreparedStatement, idx: Int, value: T): Unit
}
object SqlParamType {
  @inline def apply [T](implicit t: SqlParamType[T]) = t
}
trait SqlResultType[T] extends SqlType[T] {
  def parse (rs: ResultSet, name: String): Option[T]
  def parse (rs: ResultSet, index: Int): Option[T]
  def parseOr (rs: ResultSet, name: String, error: String): ParseResult[T] = parse(rs, name) map (x => Right(x)) getOrElse Left(error)
  def parseOr (rs: ResultSet, index: Int, error: String): ParseResult[T] = parse(rs, index) map (x => Right(x)) getOrElse Left(error) 
  def apply (name: String) = new ExprParser(name)(this)
}
object SqlResultType {
  @inline def apply [T](implicit t: SqlResultType[T]) = t
}

trait SqlMappedType [T] extends SqlParamType[T] with SqlResultType[T] {self =>
  def tpe: Int // jdbc sql type
}
  
sealed trait Sql {
  def sql: String
}

sealed trait SqlExpr [X] extends Sql {self =>
  implicit def paramTpe: SqlParamType[X]
  
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
  
  def as (alias: String)(implicit ev: SqlResultType[X]): SqlNamedExpr[X] = new SqlNamedExprImpl[X] (
    name  = alias,
    exprSql   = sql,
    params = params
  )

  def desc  = SqlOrdering(this, Descending)
  def asc   = SqlOrdering(this, Ascending)

  type TypeMapper[A,B] = SqlType[A] => SqlType[B]
  type ExprMapper[A,B,C] = SqlExpr[A] => SqlExpr[B] => SqlExpr[C]
}

class SqlNativeType[T] (
  val tpe: Int, 
  getByName: (ResultSet, String) => T,
  getByIndex: (ResultSet, Int) => T,
  _set: (PreparedStatement, Int, T) => Unit
) extends SqlMappedType [T] {
  def set (stmt: PreparedStatement, idx: Int, value: T): Unit = {
    if (value==null) stmt.setNull(idx, tpe)
    else _set(stmt, idx, value)
  }
  def parse (rs: ResultSet, name: String) = Option(getByName(rs, name)) filter {_ => !rs.wasNull}
  def parse (rs: ResultSet, idx: Int) = Option(getByIndex(rs, idx)) filter {_ => !rs.wasNull}
}

class SqlCustomType[T,N] (from: N => T, to: T => N)(implicit nt: SqlNativeType[N]) extends SqlMappedType[T] {
  def tpe = nt.tpe
  def parse (rs: ResultSet, name: String) = nt.parse(rs, name) map from
  def parse (rs: ResultSet, idx: Int) = nt.parse(rs, idx) map from
  def set (stmt: PreparedStatement, idx: Int, value: T): Unit = nt.set(stmt, idx, to(value))
}

object SqlExpr {
  implicit def intToSqlLongLit (base: Int): SqlExpr[Long] = SqlLiteralExpr(base: Long)
}

class SqlWrappedExpr [T,X:SqlParamType] (sqlExpr: SqlExpr[T]) extends SqlExpr[X] {
  val paramTpe = SqlParamType[X]
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

  def selectSql = sql + " as " + name

  def expressions = List(new query.SelectExprS(selectSql, params))

  // this should only be applicable to sub-queries
  def apply [X:SqlMappedType](column: String) = new SqlRawExpr[X](name+"."+column)
  def apply [X:SqlMappedType](col: SqlNonNullableCol[X]): SqlNamedExpr[X] = new SqlNamedExprImpl[X](
    name    = col.name,
    exprSql     = self.name+"."+col.name,
    params  = col.params
  )
  def apply [X:SqlMappedType](col: SqlNamedExpr[X]) = new SqlRawExpr[X](name+"."+col.name).as(col.name)
}

object SqlNamedExpr {
  implicit def fromCol [T](c: SqlNonNullableCol[T]): SqlNamedExpr[T] = new SqlNamedExprImpl[T] (
    name    = c.name, 
    exprSql     = c.sql,
    params  = c.params
  )(c.paramTpe, c.resultTpe)

  def apply [X:SqlParamType:SqlResultType] (name: String, sql: String, params: Seq[SqlParam[_]]): SqlNamedExpr[X] = new SqlNamedExprImpl(name, sql, params)
}

private [scoop] class SqlNamedExprImpl [T:SqlParamType:SqlResultType] (val name: String, exprSql: String, val params: Seq[SqlParam[_]]) 
    extends SqlExpr[T] with SqlNamedExpr[T] with Selection[T] {self =>
  val paramTpe = SqlParamType[T]
  val resultTpe = SqlResultType[T]

  def sql = exprSql

  def as (alias: String): SqlNamedExpr[T] = new SqlNamedExprImpl [T] (alias, sql, params)
  def parse (rs: ResultSet) = implicitly[SqlResultType[T]].parse(rs, name)

  def apply (rs: ResultSet) = parse(rs).toRight("Column [" + name + "] produced by [" + this + "] was not found in ResultSet: " + util.inspectRS(rs))
}

private [scoop] class SqlNamedQueryExpr[T:SqlParamType:SqlResultType] (queryExpr: SqlQueryExpr[T], name: String) 
  extends SqlNamedExprImpl[T](name, queryExpr.sql, queryExpr.params) {

  def on (pred: SqlExpr[Boolean]) = query.Join(selectSql, pred.sql, params ++ pred.params)
}

/**
 * Typed query with one column
 */
case class SqlQueryExpr[T:SqlParamType] (query: com.gravitydev.scoop.query.Query) extends SqlExpr[T] {
  def paramTpe = SqlParamType[T]
  override def sql = "(" + util.formatSubExpr(query.sql) + ")"
  def params = query.params
  override def as (name: String)(implicit t: SqlResultType[T]) = new SqlNamedQueryExpr[T](this, name)
}

private [scoop] class SqlRawParamExpr [X:SqlParamType] (val sql: String, val params: Seq[SqlParam[_]]) extends SqlExpr[X] {
  def paramTpe = SqlParamType[X]
}
private [scoop] class SqlRawResultExpr [X:SqlResultType] (sqlExpr: query.SqlS) {
  def sql = sqlExpr.sql
  def params = sqlExpr.params
}

class SqlRawExpr[X:SqlParamType:SqlResultType] (val sql: String, val params: Seq[SqlParam[_]] = Nil) extends SqlExpr[X] {
  val paramTpe = SqlParamType[X]
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

  def col[T:SqlMappedType](name: Symbol, cast: String = null) = new SqlNonNullableCol[T](name.name, Option(cast), this)
  
  def as (alias: String): T = {
    val t = _companion.apply
    t.__alias = alias
    t
  }
  
  // so it can serve as a companion
  def apply (): T = this

  // generate a column alias
  def apply [X:SqlParamType:SqlResultType](column: String) = new SqlRawExpr[X](_alias+"."+column)
  
  def sql = if (_tableName == _alias) _tableName else _tableName + " as " + _alias
  def updateSql = _tableName
}

class SqlAssignment [T:SqlParamType](val col: SqlCol[T], value: SqlExpr[T]) extends SqlExpr[Unit] {
  // hack
  val paramTpe = null
  // assignments should have only the table name, never the alias
  def sql = col.columnName + " = " + valueSql
  def valueSql = value.sql + col.cast.map("::" + _).getOrElse("")
  override def params = value.params
}

case class SqlInfixExpr [T:SqlParamType](l: SqlExpr[_], r: SqlExpr[_], op: String) extends SqlExpr[T] {
  val paramTpe = SqlParamType[T]
  def params = l.params ++ r.params
  def sql = "(" + l.sql + " " + op + " " + r.sql + ")"
  override def toString = "SqlInfixExpr(sql=" + sql + ", params=" + renderParams(params) + ")"
}

/**
 * @param name Name of the column to parse out of the ResultSet (might be an alias)
 */
sealed abstract class SqlCol[T:SqlParamType:SqlResultType] (val cast: Option[String], table: SqlTable[_], explicitAlias: String = null) extends SqlExpr[T] {
  val paramTpe = SqlParamType[T]

  def columnName: String

  def name: String = Option(explicitAlias) getOrElse table._prefix + columnName

  val params = Nil

  def sql = table._alias + "." + columnName + cast.map(_=>"::varchar").getOrElse("") // TODO: use correct base type

  def expressions = List( new query.SelectExprS(sql + " as " + name) )
}

class SqlNonNullableCol[T:SqlMappedType](val columnName: String, cast: Option[String], table: SqlTable[_], explicitAlias: String = null) 
    extends SqlCol[T] (cast, table, explicitAlias) with Selection[T] {

  val resultTpe = SqlResultType[T]
  override def toString = "Col(" + columnName + " as " + name + ")"

  def apply (rs: ResultSet) = resultTpe.parseOr(rs, name, "Column ["+name+"] not found in ResultSet: "+util.inspectRS(rs))

  def nullable = new SqlNullableCol[T](columnName, cast, table, explicitAlias)
  def := (x: SqlExpr[T]) = new SqlAssignment(this, x)
}

class SqlNullableCol[T:SqlMappedType](val columnName: String, cast: Option[String], table: SqlTable[_], explicitAlias: String) 
    extends SqlCol[T] (cast, table, explicitAlias) with Selection[Option[T]] {

  val resultTpe = SqlResultType[T]
  override def toString = "Col("+ columnName + " as " + name +" : Nullable)"

  def apply (rs: ResultSet) = resultTpe.parse(rs, name) map (x => Right(Some(x))) getOrElse Right(None)

  def := (x: Option[SqlExpr[T]]) = new SqlAssignment(this, x getOrElse new SqlRawExpr[T]("NULL"))
}

case class SqlUnaryExpr [L:SqlParamType,T:SqlParamType](l: SqlExpr[L], op: String, postfix: Boolean) extends SqlExpr [T] {
  val paramTpe = SqlParamType[T]
  def params = l.params
  def sql = "(" + (if (postfix) l.sql + " " + op else op + " " + l.sql) + ")"
}

case class SqlLiteralExpr [T:SqlParamType] (v: T) extends SqlExpr[T] {
  val paramTpe = SqlParamType[T]
  override def params = List(SqlSingleParam(v))
  def sql = "?"
}

case class SqlLiteralSetExpr [T:SqlParamType] (v: Set[T]) extends SqlExpr[Set[T]] {
  val paramTpe = SqlParamType[Set[T]]
  override def params = SqlSetParam(v).toList
  def sql = v.toList.map(_ => "?").mkString("(", ", ", ")")
}

