package com.gravitydev.scoop

import java.sql.{ResultSet, PreparedStatement, Types, Timestamp, Date}
import ast.{SqlParamType, SqlResultType, SqlNamedExpr, SqlNamedReqExpr, SqlNamedOptExpr, SqlNonNullableCol, SqlNullableCol}
import parsers.{OptionalExprParser, ParserBase, SelectionBase}

object `package` {
  // aliases for convenience
  type Table[T <: ast.SqlTable[T]] = ast.SqlTable[T]
  type TableCompanion[T <: Table[T]] = {def apply() : T}

  type SqlExpr[T]         = ast.SqlExpr[T]
  type SqlNativeType[T]   = ast.SqlNativeType[T]
  type SqlCustomType[I,O] = ast.SqlCustomType[I,O]

  type ExprParser[T]      = parsers.ExprParser[T]
  type ResultSetParser[T] = parsers.ResultSetParser[T]
  //type Selection[T]       = parsers.Selection[T]

  type ParseResult[+T] = Either[String,T]

  @deprecated("Use ast.SqlMappedType if you need to (you shouldn't)", "0.1.23-SNAPSHOT")
  type SqlType[T] = ast.SqlMappedType[T]

  // native types
  implicit val sqlInt        = new SqlNativeType [Int]          (Types.INTEGER,   _ getInt _,       _ getInt _,        _ setInt (_,_))  
  implicit val sqlLong       = new SqlNativeType [Long]         (Types.BIGINT,    _ getLong _,      _ getLong _,       _ setLong (_,_))
  implicit val sqlDouble     = new SqlNativeType [Double]       (Types.DOUBLE,    _ getDouble _,    _ getDouble _,     _ setDouble (_,_))
  implicit val sqlString     = new SqlNativeType [String]       (Types.VARCHAR,   _ getString _,    _ getString _,     _ setString (_,_))
  implicit val sqlTimestamp  = new SqlNativeType [Timestamp]    (Types.TIMESTAMP, _ getTimestamp _, _ getTimestamp _,  _ setTimestamp (_,_))
  implicit val sqlDate       = new SqlNativeType [Date]         (Types.DATE,      _ getDate _,      _ getDate _,       _ setDate (_,_))
  implicit val sqlBoolean    = new SqlNativeType [Boolean]      (Types.BOOLEAN,   _ getBoolean _,   _ getBoolean _,    _ setBoolean (_,_))

  implicit val sqlDecimal    = new SqlNativeType [BigDecimal] (
    Types.DECIMAL, 
    (rs, idx) => BigDecimal(rs.getBigDecimal(idx)), 
    (rs, idx) => BigDecimal(rs.getBigDecimal(idx)), 
    (rs, idx, value) => rs.setBigDecimal(idx, value.underlying)
  )

  implicit val sqlAnyref     = new SqlNativeType [AnyRef] (
    Types.JAVA_OBJECT, 
    _ getObject _, 
    _ getObject _, 
    _ setObject(_,_)
  )


  def opt [T](p: ResultSetParser[T]): ParserBase[Option[T]] = {
    new ParserBase [Option[T]] (rs => p(rs).fold(_ => Right(None), x => Right(Option apply x))) {} // TODO: isn't there a concrete class for this?
  }

  def opt [T](p: Selection[T]): SelectionBase[Option[T]] = {
    new SelectionBase [Option[T]] (rs => p(rs).fold(_ => Right(None), x => Right(Option apply x))) {
      def expressions = p.expressions
    }
  }

  def req [T](p: ResultSetParser[Option[T]]): ParserBase[T] = {
    new ParserBase[T] (rs => p(rs) fold (
      error => sys.error("Required value not found: " + error),
      s => s map (v => Right(v)) getOrElse Left("Could not parse value from parser: " + p)
    )) {} // TODO: concrete class here
  }

  def req [T](p: Selection[Option[T]]): SelectionBase[T] = {
    new SelectionBase[T] (rs => p(rs) fold (
      error => sys.error("Required value not found: " + error),
      s => s map (v => Right(v)) getOrElse Left("Could not parse value from parser: " + p)
    )) {
      def expressions = p.expressions
    }
  }

  // TODO: figure out how to not need this
  //implicit def wrapParser [T](parser: parsers.ResultSetParser[T]) = new parsers.ParserWrapper(parser)
  implicit def wrapSelection[T](selection: Selection[T]) = new parsers.Selection1(selection)
  
  // TODO: clean up these hacks
  implicit def setT[X:ast.SqlParamType] = new SqlNativeType [Set[X]] (
    -1, 
    (_,_) => sys.error("internal"), 
    (_,_) => sys.error("internal"), 
    (_,_,_) => sys.error("internal")
  )
 
  // TODO: handle params

  //implicit def toParser [X:SqlResultType] (c: SqlNamedReqExpr[X]) = new ExprParser[X](c.name, List(new query.SqlFragmentS(c.sql, c.params)))
  //implicit def toSqlReqNamedExpr [X:SqlParamType:SqlResultType] (c: SqlNamedReqExpr[X]) = new SqlNamedReqExpr[X](c.name, c.sql, c.params)

  implicit def namedReqExprToSelection [X:SqlResultType] (expr: SqlNamedReqExpr[X]): Selection[X] = Selection.required[X](expr.name, expr.sql, expr.params)

  //implicit def toParser2 [X:SqlResultType] (c: SqlNonNullableCol[X]) = new ExprParser[X](c.name, List(c.sql + " as " + c.name))
  //implicit def toParser2 [X:SqlParamType:SqlResultType] (c: SqlNonNullableCol[X]): ResultSetParser[X] = new SqlNamedReqExpr[X](c.name, c.sql + " as " + c.name, c.params)

  implicit def toOptParser [X:SqlResultType] (c: SqlNamedOptExpr[X]) = new OptionalExprParser[X](c.name, List(new query.SqlFragmentS(c.sql, c.params)))
  implicit def toOptParser2 [X:SqlResultType] (c: SqlNullableCol[X]) = new OptionalExprParser[X](c.name, List(c.sql + " as " + c.name))
  
  private[scoop] def renderParams (params: Seq[SqlParam[_]]) = params.map(x => x.v + ":"+x.v.asInstanceOf[AnyRef].getClass.getName.stripPrefix("java.lang."))
}

object Selection {
  def required [T:SqlResultType] (name: String, sql: String, params: Seq[SqlParam[_]]) = new SelectionBase[T] (
    rs => SqlResultType[T].parseOr(rs, name, "Could not parse expression: " + name + " [" + SqlResultType[T] + "] from " + util.inspectRS(rs))
  ) with Selection [T] {
    lazy val expressions = List( new query.SelectExprS(sql + " as " + name, params) )
  }
}

/**
 * ResultSet parser that accumulates a list of expressions to be included in a SELECT clause
 */
trait Selection [+T] extends (ResultSet => ParseResult[T]) with parsers.ResultSetParserFlatMap[T] {self =>
  def map [X] (fn: T => X): Selection[X] = new parsers.MappedSelection(self, fn)
  def expressions: List[query.SelectExprS]
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
  def apply (rs: ResultSet) = Right(value)
}

