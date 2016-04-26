package com.gravitydev.scoop

import java.sql.{ResultSet, PreparedStatement, Types, Timestamp, Date}
import ast.{SqlNamedExpr, SqlNamedQuery, SqlParseStrictExpr/*, SqlNonNullableCol, SqlNullableCol*/, SqlUnderlyingType}
import parsers.Selection1

object `package` {
  // aliases for convenience
  type Table[T <: definition.Table[T]] = definition.Table[T]
  type TableCompanion[T <: Table[T]] = {def apply() : T}

  type SqlType[T]         = ast.SqlType[T]
  type SqlExpr[T]         = ast.SqlExpr[T]
  type SqlNativeType[T]   = ast.SqlNativeType[T]
  //type SqlCustomType[I,O] = ast.SqlCustomType[I,O]

  type ExprParser[T]      = parsers.ExprParser[T]
  type ResultSetParser[T] = parsers.ResultSetParser[T]

  type ParseResult[+T] = Either[String,T]

  // native types
  implicit val sqlInt        = new SqlNativeType [Int]          (Types.INTEGER,   _ getInt _,       _ getInt _,        _ setInt (_,_))  
  implicit val sqlLong       = new SqlNativeType [Long]         (Types.BIGINT,    _ getLong _,      _ getLong _,       _ setLong (_,_))
  implicit val sqlDouble     = new SqlNativeType [Double]       (Types.DOUBLE,    _ getDouble _,    _ getDouble _,     _ setDouble (_,_))
  implicit val sqlString     = new SqlNativeType [String]       (Types.VARCHAR,   _ getString _,    _ getString _,     _ setString (_,_))
  implicit val sqlTimestamp  = new SqlNativeType [Timestamp]    (Types.TIMESTAMP, _ getTimestamp _, _ getTimestamp _,  _ setTimestamp (_,_))
  implicit val sqlDate       = new SqlNativeType [Date]         (Types.DATE,      _ getDate _,      _ getDate _,       _ setDate (_,_))
  implicit val sqlBoolean    = new SqlNativeType [Boolean]      (Types.BOOLEAN,   _ getBoolean _,   _ getBoolean _,    _ setBoolean (_,_))

  // must deal with null, since we are trying to map it to a scala type
  implicit val sqlDecimal    = new SqlNativeType [BigDecimal] (
    Types.DECIMAL, 
    (rs, idx) => {
      val res = rs.getBigDecimal(idx)
      if (res == null) null else BigDecimal(res)
    },
    (rs, idx) => {
      val res = rs.getBigDecimal(idx)
      if (res == null) null else BigDecimal(res)
    }, 
    (rs, idx, value) => rs.setBigDecimal(idx, value.underlying)
  )

  implicit val sqlAnyRef     = new SqlNativeType [AnyRef] (
    Types.JAVA_OBJECT, 
    _ getObject _, 
    _ getObject _, 
    _ setObject(_,_)
  )

  class CustomTypeBuilder [T,X] {
    def apply [N](from: X=>T, to: T=>X)(implicit ev: SqlType[X] with SqlUnderlyingType[N]) = new ast.SqlWrappedType(from, to)(ev)
  }
  def customType [T,X] = new CustomTypeBuilder[T,X]

  def opt [T](p: Selection[T]): Selection1[Option[T]] = new Selection1 [Option[T]] (
    rs => p(rs).fold(_ => Right(None), x => Right(Option apply x)), 
    p.expressions
  )

  def req [T](p: Selection[Option[T]]): Selection1[T] = {
    new Selection1[T] (
      rs => p(rs) fold (
        error => sys.error("Required value not found: " + error),
        s => s map (v => Right(v)) getOrElse Left("Could not parse value from selection: " + p)
      ),
      p.expressions
    )
  }

  // TODO: clean up these hacks
  implicit def setT[X:SqlType] = new SqlNativeType [Set[X]] (
    -1, 
    (_,_) => sys.error("internal"), 
    (_,_) => sys.error("internal"), 
    (_,_,_) => sys.error("internal")
  )

  //implicit def nonNullableColToSelection [X] (col: SqlNonNullableCol[X]): Selection1[X] = new Selection1 (col, col.expressions)
  //implicit def nullableColToSelection [X] (col: SqlNullableCol[X]): Selection1[Option[X]] = new Selection1 (col, col.expressions)

  /** Turns a named query into an named expression (Selection) */
  implicit def namedQueryToExpr[T:SqlType](q: SqlNamedQuery[T]): ast.SqlNamedQueryExpr[T] = new ast.SqlNamedQueryExpr(q.query, q.name)

  /** Turns a query into an expression */
  implicit def queryToExpr[T:SqlType](q: ast.Query[T]): ast.SqlExpr[T] = new ast.SqlQueryExpr(q) 

  implicit def exprToParseExpr [T](expr: SqlExpr[T]): SqlParseStrictExpr[T] = new SqlParseStrictExpr [T] {
    val sqlTpe = expr.sqlTpe
    //val sql = expr.sql
  }
  
  private[scoop] def renderParams (params: Seq[SqlParam[_]]) = params.map(x => x.v + ":"+x.v.asInstanceOf[AnyRef].getClass.getName.stripPrefix("java.lang."))
}

/**
 * ResultSet parser that accumulates a list of expressions to be included in a SELECT clause
 */
trait Selection [+T] extends (ResultSet => ParseResult[T]) with parsers.ResultSetParserFlatMap[T] {self =>
  def map [X] (fn: T => X): Selection[X] = new parsers.MappedSelection(self, fn)
  def expressions: Seq[ast.SqlNamedExpr[_,_]]
  //override def toString = "Selection(" + expressions.map(_.toString).mkString(", ") + ")"
}

sealed trait SqlParam [T] {
  val v: T
  def apply (stmt: PreparedStatement, idx: Int): Unit
}

case class SqlSingleParam [T,S] (v: T)(implicit val tp: ast.SqlType[T]) extends SqlParam[T] {
  override def toString = v.toString + ":" + v.getClass
  def apply (stmt: PreparedStatement, idx: Int) = tp.set(stmt, idx, v)
}
case class SqlSetParam [T](v: Set[T])(implicit tp: ast.SqlType[T]) extends SqlParam[Set[T]] {
  def toList = v.toList.map(x => SqlSingleParam(x))
  def apply (stmt: PreparedStatement, idx: Int) = sys.error("WTF!")
}

case class literal [T] (value: T) extends Selection [T] {
  val expressions = Nil
  def apply (rs: ResultSet) = Right(value)
}

