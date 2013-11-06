package com.gravitydev.scoop

import java.sql.{ResultSet, PreparedStatement, Types, Timestamp, Date}
import ast.{SqlParamType, SqlResultType, SqlNamedReqExpr, SqlNamedOptExpr, SqlNonNullableCol, SqlNullableCol}
import parsers.{ParseSuccess, ParseFailure, OptionalExprParser, ParserBase, ResultSetParser}

object `package` {
  // aliases for convenience
  type Table[T <: ast.SqlTable[T]] = ast.SqlTable[T]
  type TableCompanion[T <: Table[T]] = {def apply() : T}

  type SqlExpr[T]         = ast.SqlExpr[T]
  type SqlNativeType[T]   = ast.SqlNativeType[T]
  type SqlCustomType[I,O] = ast.SqlCustomType[I,O]

  type ExprParser[T] = parsers.ExprParser[T]

  @deprecated("Use ast.SqlMappedType if you need to (you shouldn't)", "0.1.23-SNAPSHOT")
  type SqlType[T] = ast.SqlMappedType[T]

  // native types
  implicit val int        = new SqlNativeType [Int]          (Types.INTEGER,   _ getInt _,       _ setInt (_,_))  
  implicit val long       = new SqlNativeType [Long]         (Types.BIGINT,    _ getLong _,      _ setLong (_,_))
  implicit val double     = new SqlNativeType [Double]       (Types.DOUBLE,    _ getDouble _,    _ setDouble (_,_))
  implicit val string     = new SqlNativeType [String]       (Types.VARCHAR,   _ getString _,    _ setString (_,_))
  implicit val timestamp  = new SqlNativeType [Timestamp]    (Types.TIMESTAMP, _ getTimestamp _, _ setTimestamp (_,_))
  implicit val date       = new SqlNativeType [Date]         (Types.DATE,      _ getDate _,      _ setDate (_,_))
  implicit val boolean    = new SqlNativeType [Boolean]      (Types.BOOLEAN,   _ getBoolean _,   _ setBoolean (_,_))
  implicit val decimal    = new SqlNativeType [BigDecimal]   (Types.DECIMAL, (rs, idx) => BigDecimal(rs.getBigDecimal(idx)), (rs, idx, value) => rs.setBigDecimal(idx, value.underlying))
  implicit val anyref     = new SqlNativeType [AnyRef]       (Types.JAVA_OBJECT, _ getObject _, _ setObject(_,_))

  def opt [T](p: ResultSetParser[T]): ParserBase[Option[T]] = {
    new ParserBase [Option[T]] (rs => p(rs) match {
      case ParseSuccess(s) => ParseSuccess(Option(s))
      case ParseFailure(e) => ParseSuccess(None)
    }) {
      def columns = p.columns
    }
  }

  // TODO: figure out how to not need this
  implicit def wrapParser [T](parser: parsers.ResultSetParser[T]) = new parsers.ParserWrapper(parser)
  
  // TODO: clean up these hacks
  implicit def setT[X:ast.SqlParamType] = new SqlNativeType [Set[X]] (-1, (_,_) => sys.error("internal"), (_,_,_) => sys.error("internal"))
 
  // TODO: handle params
  implicit def toParser [X:SqlResultType] (c: SqlNamedReqExpr[X]) = new ExprParser[X](c.name, List(new query.SqlFragmentS(c.sql, c.params)))
  implicit def toParser2 [X:SqlResultType] (c: SqlNonNullableCol[X]) = new ExprParser[X](c.name, List(c.sql + " as " + c.name))
  implicit def toOptParser [X:SqlResultType] (c: SqlNamedOptExpr[X]) = new OptionalExprParser[X](c.name, List(new query.SqlFragmentS(c.sql, c.params)))
  implicit def toOptParser2 [X:SqlResultType] (c: SqlNullableCol[X]) = new OptionalExprParser[X](c.name, List(c.sql + " as " + c.name))
  
  private[scoop] def renderParams (params: Seq[SqlParam[_]]) = params.map(x => x.v + ":"+x.v.asInstanceOf[AnyRef].getClass.getName.stripPrefix("java.lang."))
}

sealed trait SqlParam [T] {
  val v: T
  def apply (stmt: PreparedStatement, idx: Int): Unit
}

case class SqlSingleParam [T,S] (v: T)(implicit val tp: ast.SqlParamType[T]) extends SqlParam[T] {
  override def toString = v.toString + ":" + v.getClass
  def apply (stmt: PreparedStatement, idx: Int) = tp.set(stmt, idx, v)
}
case class SqlSetParam [T](v: Set[T])(implicit tp: ast.SqlParamType[T]) extends SqlParam[Set[T]] {
  def toList = v.toList.map(x => SqlSingleParam(x))
  def apply (stmt: PreparedStatement, idx: Int) = sys.error("WTF!")
}

case class literal [T] (value: T) extends ResultSetParser [T] {
  def columns = Nil
  def apply (rs: ResultSet) = ParseSuccess(value)
}

