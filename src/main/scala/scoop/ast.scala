package com.gravitydev.scoop 
package ast

import java.sql.{Connection, PreparedStatement, ResultSet}
import parsers.ResultSetParser
import query.ParameterizedSql
import util.QueryResult

object `package` {
  // convenience
  type TableT = ast.SqlTable[T] forSome {type T <: ast.SqlTable[T]}
}

sealed trait QueryNode {
  /** A comment that will be included in the generated sql - useful for debugging */
  var _comment: String = ""
  def comment(s: String) = {_comment = s; this}
}

sealed trait Queryable extends QueryNode

case class Join (queryable: Queryable, predicate: SqlExpr[Boolean], joinType: JoinType) extends QueryNode
sealed abstract class JoinType (val sql: String)
object JoinType {
  case object Inner extends JoinType("INNER")
  case object Left extends JoinType("LEFT")
}

sealed abstract class SqlOrder (val sql: String)
object SqlOrder {
  case object Ascending   extends SqlOrder ("ASC")
  case object Descending  extends SqlOrder ("DESC")
}
case class SqlOrdering (expr: SqlExpr[_], order: SqlOrder) extends QueryNode
object SqlOrdering {
  implicit def fromExpr (expr: ast.SqlExpr[_]): SqlOrdering = expr.asc
}

sealed trait InsertBase extends QueryNode {
  def apply ()(implicit c: Connection, dialect: SqlDialect) = {
    val parameterizedSql = dialect.toParameterizedSql(this)
    try {
      import java.sql.Statement

      util.using(c.prepareStatement(parameterizedSql.sql, Statement.RETURN_GENERATED_KEYS)) {stmt => 
        for ((p, idx) <- parameterizedSql.params.zipWithIndex) p(stmt, idx+1)
        stmt.executeUpdate()

        // TODO: this is a hack, do this right
        try util.using(stmt.getGeneratedKeys()) {rs =>
          rs.next()
          Some(rs.getLong(1))
        } catch {
          case _:Throwable => None
        }
      }
    } catch {
      case e: java.sql.SQLException => throw new Exception("SQL Exception ["+e.getMessage+"] when executing query ["+parameterizedSql.sql+"] with parameters: ["+parameterizedSql.params+"]")
    }
  }

  // upsert
  def onDuplicateKeyUpdate (assignments: builder.Assignment*) = Upsert(this, builder.toAstSqlAssignments(assignments))
}

case class Insert (
  into: String,
  assignments: Seq[ast.SqlAssignment[_]] = Nil
) extends InsertBase

// TODO
case class InsertBatch (
  into: String,
  values: List[List[String]]
)

case class Upsert (
  insert: InsertBase,
  assignments: Seq[ast.SqlAssignment[_]] = Nil
) extends InsertBase

// using a subselect
case class InsertWithQuery (
  into: String,
  columns: List[ast.SqlCol[_]],
  query: ast.Query[_]
) extends InsertBase

case class Update (
  table: TableT,
  assignments: Seq[ast.SqlAssignment[_]] = Nil,
  predicate: Option[ast.SqlExpr[Boolean]] = None
) extends QueryNode {
  def where (pred: ast.SqlExpr[Boolean]): Update = copy(predicate = predicate.map(_ && pred).orElse(Some(pred)))

  def apply ()(implicit c: Connection, dialect: SqlDialect) = {
    val parameterizedSql = dialect.toParameterizedSql(this)

    try util.using(c.prepareStatement(parameterizedSql.sql)) {stmt =>
      for ((p, idx) <- parameterizedSql.params.zipWithIndex) p(stmt, idx+1)
      stmt.executeUpdate()
    } catch {
      case e: java.sql.SQLException => throw new Exception("SQL Exception ["+e.getMessage+"] when executing query ["+parameterizedSql.sql+"] with parameters: ["+parameterizedSql.params+"]")
    }
  }
}
 
case class Delete (
  table: TableT,
  predicate: ast.SqlExpr[Boolean]
) extends QueryNode {
  def where (pred: ast.SqlExpr[Boolean]) = copy(predicate = predicate && pred)

  def apply ()(implicit c: Connection, dialect: SqlDialect) = {
    val parameterizedSql = dialect.toParameterizedSql(this)

    util.using(c.prepareStatement(parameterizedSql.sql)) {stmt =>
      for ((p, idx) <- parameterizedSql.params.zipWithIndex) p(stmt, idx+1)
      stmt.executeUpdate()
    }
  }
}

trait SqlExprIn[-T]
trait SqlExprOut[+T]

trait SqlExpr[X] extends SqlExprIn[X] with SqlExprOut[X] with QueryNode {self =>
  implicit def sqlTpe: SqlType[X]
  
  def === (v: SqlExpr[X]) = SqlInfixExpr[Boolean](this, v, "=")

  def <> (v: SqlExpr[X]) = SqlInfixExpr[Boolean](this, v, "<>")
 
  // alias
  @deprecated("Does not play nice with fixity", "Now")
  def |=| (v: SqlExpr[X]) = === (v)
  
  def <   (v: SqlExpr[X]) = SqlInfixExpr[Boolean](this, v, "<")
  def <=  (v: SqlExpr[X]) = SqlInfixExpr[Boolean](this, v, "<=")
  def >   (v: SqlExpr[X]) = SqlInfixExpr[Boolean](this, v, ">")
  def >=  (v: SqlExpr[X]) = SqlInfixExpr[Boolean](this, v, ">=")
  
  // it would be nice to have a view bound here
  def in (v: Set[X]) = if (v.isEmpty) SqlLiteralExpr(false) else SqlInfixExpr[Boolean](this, SqlLiteralSetExpr(v), "IN")
  
  def notIn (v: Set[X]) = if (v.isEmpty) SqlLiteralExpr(true) else SqlInfixExpr[Boolean](this, SqlLiteralSetExpr(v), "NOT IN")
 
  @deprecated("Use symbolic &&", "Now") 
  def and (v: SqlExpr[Boolean])(implicit ev: SqlExpr[X] =:= SqlExpr[Boolean]) = && (v)
  @deprecated("Use symbolic ||", "Now") 
  def or  (v: SqlExpr[Boolean])(implicit ev: SqlExpr[X] =:= SqlExpr[Boolean]) = || (v) 
  
  // symbolic names for better precedence rules
  def && (v: SqlExpr[Boolean])(implicit ev: SqlExpr[X] =:= SqlExpr[Boolean]) = SqlBinaryApplic.create[Boolean](this, v, "AND")
  def || (v: SqlExpr[Boolean])(implicit ev: SqlExpr[X] =:= SqlExpr[Boolean]) = SqlBinaryApplic.create[Boolean](this, v, "OR") 
  
  def isNull = SqlUnaryExpr[X,Boolean](this, "IS NULL", postfix=true)
  def isNotNull = SqlUnaryExpr[X,Boolean](this, "IS NOT NULL", postfix=true)

  def like (v: SqlExpr[String])(implicit ev: SqlType[X] with SqlUnderlyingType[String]) = SqlInfixExpr[Boolean](this, v, "LIKE")
  def notLike (v: SqlExpr[String])(implicit ev: SqlType[X] with SqlUnderlyingType[String]) = SqlInfixExpr[Boolean](this, v, "NOT LIKE")

  def cast [T:SqlType]: SqlExpr[T] = SqlWrappedExpr[T](this)
 
  // TODO: decimals
  def + [T,N](v: SqlExpr[T])(implicit ev1: SqlExpr[X]=>SqlExpr[N], ev2: SqlExpr[T]=>SqlExpr[N], ev3: SqlType[N]) = SqlInfixExpr[N](this, v, "+")
  def - [T,N](v: SqlExpr[T])(implicit ev1: SqlExpr[X]=>SqlExpr[N], ev2: SqlExpr[T]=>SqlExpr[N], ev3: SqlType[N]) = SqlInfixExpr[N](this, v, "-")
  def * [T,N](v: SqlExpr[T])(implicit ev1: SqlExpr[X]=>SqlExpr[N], ev2: SqlExpr[T]=>SqlExpr[N], ev3: SqlType[N]) = SqlInfixExpr[N](this, v, "*")
  def / [T,N](v: SqlExpr[T])(implicit ev1: SqlExpr[X]=>SqlExpr[N], ev2: SqlExpr[T]=>SqlExpr[N], ev3: SqlType[N]) = SqlInfixExpr[N](this, v, "/")
 
  def desc  = SqlOrdering(this, SqlOrder.Descending)
  def asc   = SqlOrdering(this, SqlOrder.Ascending)

  type TypeMapper[A,B] = SqlType[A] => SqlType[B]
  type ExprMapper[A,B,C] = SqlExpr[A] => SqlExpr[B] => SqlExpr[C]
}
object SqlExpr {
  implicit def intToSqlLongLit (base: Int): SqlExpr[Long] = SqlLiteralExpr(base: Long)
  implicit def queryToExpr [T:SqlType](query: Query[T]) = SqlQueryExpr[T](query)
}

/** Useful for overriding the sqlType (i.e. SqlExpr[Int] => SqlExpr[Long]) TODO: is there a better way? */
case class SqlWrappedExpr[T:SqlType](expr: SqlExpr[_]) extends SqlExpr[T] {
  def sqlTpe = SqlType[T]
}

/** Expression that will produce a strict value when parsed out of a ResultSet */
trait SqlParseStrictExpr[T] extends SqlExpr[T] {
  def as (alias: String): SqlNamedStrictExpr[T] = SqlNamedStrictExpr[T](this, alias)
}

/** Expression that will produce an optional value when parsed out of a ResultSet */
trait SqlParseOptExpr[T] extends SqlExpr[T] {
  def as (alias: String): SqlNamedOptExpr[T] = new SqlNamedOptExpr[T](this, alias)
}

/** Just a convenience function that holds the sqlType */
private [scoop] abstract class SqlBaseExpr[T:SqlType] extends SqlParseStrictExpr[T] {
  val sqlTpe = SqlType[T]
}

/** Query with one column */
case class SqlQueryExpr[I:SqlType] (query: Query[I]) extends SqlExpr[I] {
  val sqlTpe = SqlType[I]
  def as (alias: String) = SqlNamedQueryExpr[I](this, alias)
}

case class SqlRawExpr[X:SqlType] (sql: ParameterizedSql) extends SqlBaseExpr[X]
case class SqlRawOptExpr[X:SqlType] (sql: ParameterizedSql) extends SqlParseOptExpr[X] {
  val sqlTpe = SqlType[X]
}

abstract class SqlTable [T <: SqlTable[T]](_companion: TableCompanion[T], tableName: String = null, schema: String = null) extends Queryable {self: T =>
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
  def apply [X:SqlType](column: String) = SqlRawExpr[X](_alias+"."+column)
  
  def fromSql = if (_tableName == _alias) _tableName else _tableName + " as " + _alias
  def updateSql = _tableName
 
  override def toString = "SqlTable(" + fromSql + ")"
}

case class SqlAssignment [T:SqlType](col: SqlCol[T], value: SqlExpr[T]) extends SqlExpr[Unit] {
  // hack
  val sqlTpe = null
}

case class SqlInfixExpr [T:SqlType](l: SqlExpr[_], r: SqlExpr[_], op: String) extends SqlBaseExpr[T]

case class SqlBinaryApplic [T:SqlType](values: Seq[SqlExpr[T]], op: String) extends SqlBaseExpr[T]
object SqlBinaryApplic {
  def create [T:SqlType](a: SqlExpr[T], b: SqlExpr[T], op: String) = a match {
    case a @ SqlBinaryApplic(values, op2) if op2 == op => a.copy(values = values ++ Seq(b), op)
    case x => SqlBinaryApplic(Seq(a, b), op)
  }
}

/**
 * @param name Name of the column to parse out of the ResultSet (might be an alias)
 */
sealed abstract class SqlCol[T:SqlType] (val cast: Option[String], val table: SqlTable[_], explicitAlias: String = null) extends SqlExpr[T] {
  val sqlTpe = SqlType[T]

  def columnName: String

  def name: String = Option(explicitAlias) getOrElse table._prefix + columnName

  val params = Nil

  def expressions: Seq[ast.SqlNamedExpr[_,_]] // = List( new query.SelectExprS(sql + " as " + name) )
}

class SqlNonNullableCol[T:SqlType](val columnName: String, cast: Option[String], table: SqlTable[_], explicitAlias: String = null) 
    extends SqlCol[T] (cast, table, explicitAlias) with SqlParseStrictExpr[T] with SqlNamedExpr[T,T] with Selection[T] {

  override def toString = "Col(" + columnName + " as " + name + ")"

  def parse (rs: ResultSet): Option[T] = SqlType[T].parse(rs, name)

  def nullable = new SqlNullableCol[T](columnName, cast, table, explicitAlias)
  def := (x: SqlExpr[T]) = SqlAssignment(this, x)
}

class SqlNullableCol[T:SqlType](val columnName: String, cast: Option[String], table: SqlTable[_], explicitAlias: String) 
    extends SqlCol[T] (cast, table, explicitAlias) with SqlParseOptExpr[T] with SqlNamedExpr[T,Option[T]] with Selection[Option[T]] {

  //override def toString = "Col("+ columnName + " as " + name +" : Nullable)"

  def parse (rs: ResultSet): Option[Option[T]] = Some(SqlType[T].parse(rs, name))

  def := (x: Option[SqlExpr[T]]) = SqlAssignment(this, x getOrElse SqlRawExpr[T]("NULL"))
}

case class SqlUnaryExpr [L:SqlType,T:SqlType](l: SqlExpr[L], op: String, postfix: Boolean) extends SqlBaseExpr [T]

case class SqlLiteralExpr [T:SqlType] (v: T) extends SqlBaseExpr[T] {
  val toParam = SqlSingleParam(v)
}

case class SqlLiteralSetExpr [T:SqlType] (v: Set[T]) extends SqlBaseExpr[Set[T]] {
  def toParams = SqlSetParam(v).toList
}

case class Query [T](
  sel:          Selection[T],
  from:         Option[Queryable]     = None,
  joins:        Seq[Join]             = Nil,
  predicate:    Option[ast.SqlExpr[Boolean]]  = None,
  order:        Seq[SqlOrdering]      = Nil,
  group:        Seq[ast.SqlExpr[_]]   = Nil,
  limit:        Option[Int]           = None,
  offset:       Option[Int]           = None,
  selectDistinct: Boolean             = false,
  forUpdateLock: Boolean              = false
) extends QueryNode {

  def forUpdate () = copy(forUpdateLock = true)
  def distinct = copy(selectDistinct=true)

  def as (alias: String) = new ast.SqlNamedQuery[T](this, alias) 

  def process [B](rowParser: ResultSet => ParseResult[B])(implicit c: Connection, dialect: SqlDialect): QueryResult[B] = {
    val parameterizedSql = dialect.toParameterizedSql(this)

    new QueryResult(query.executeQuery(parameterizedSql)(rowParser))
  }

  def union (q: ParameterizedSql)(implicit dialect: SqlDialect) = dialect.toParameterizedSql(this) union q  
}

private [scoop] case class SqlNamedQuery [T](val query: Query[T], val name: String) extends Queryable with SqlNamed {
  // generate a column alias
  def apply [X:SqlType](column: String) = SqlRawExpr[X](name+"."+column)
  def apply [X](col: ast.SqlNamedOptExpr[X]): ast.SqlNamedOptExpr[X] = ast.SqlRawOptExpr[X](name+"."+col.name)(col.sqlTpe).as(col.name)
  def apply [X](col: ast.SqlNamedStrictExpr[X]): ast.SqlNamedStrictExpr[X] = ast.SqlRawExpr[X](name+"."+col.name)(col.sqlTpe).as(col.name)

  def on (pred: SqlExpr[Boolean]) = new builder.JoinBuilder(this, pred)
}

/** Named query expression (returns one column) */
private [scoop] case class SqlNamedQueryExpr[I:SqlType] (queryExpr: SqlQueryExpr[I], val name: String) extends Queryable with SqlNamedExpr[I,I] { 
  val sqlTpe = SqlType[I] 
  def parse (rs: ResultSet) = SqlType[I].parse(rs, name) 
  def on (pred: SqlExpr[Boolean]) = new builder.JoinBuilder(this, pred)
}

