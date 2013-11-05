package com.gravitydev.scoop

import java.sql.{ResultSet, PreparedStatement, Types, Timestamp, Date}

object `package` {
  type Table[T <: ast.SqlTable[T]] = ast.SqlTable[T]
  type TableCompanion[T <: Table[T]] = {def apply() : T}

  @deprecated("Use ast.SqlMappedType", "0.1.23-SNAPSHOT")
  type SqlType[T] = ast.SqlMappedType[T]

  type SqlNativeType[T] = ast.SqlNativeType[T]
  type SqlCustomType[I,O] = ast.SqlCustomType[I,O]

  implicit val int        = new SqlNativeType [Int]          (Types.INTEGER,   _ getInt _,       _ setInt (_,_))  
  implicit val long       = new SqlNativeType [Long]         (Types.BIGINT,    _ getLong _,      _ setLong (_,_))
  implicit val double     = new SqlNativeType [Double]       (Types.DOUBLE,    _ getDouble _,    _ setDouble (_,_))
  implicit val string     = new SqlNativeType [String]       (Types.VARCHAR,   _ getString _,    _ setString (_,_))
  implicit val timestamp  = new SqlNativeType [Timestamp]    (Types.TIMESTAMP, _ getTimestamp _, _ setTimestamp (_,_))
  implicit val date       = new SqlNativeType [Date]         (Types.DATE,      _ getDate _,      _ setDate (_,_))
  implicit val boolean    = new SqlNativeType [Boolean]      (Types.BOOLEAN,   _ getBoolean _,   _ setBoolean (_,_))
  implicit val decimal    = new SqlNativeType [BigDecimal]   (Types.DECIMAL, (rs, idx) => BigDecimal(rs.getBigDecimal(idx)), (rs, idx, value) => rs.setBigDecimal(idx, value.underlying))
  implicit val anyref     = new SqlNativeType [AnyRef]       (Types.JAVA_OBJECT, _ getObject _, _ setObject(_,_))

  def opt [T](p: ResultSetParser[T]): boilerplate.ParserBase[Option[T]] = {
    new boilerplate.ParserBase [Option[T]] (rs => p(rs) match {
      case ParseSuccess(s) => ParseSuccess(Option(s))
      case ParseFailure(e) => ParseSuccess(None)
    }) {
      def columns = p.columns
    }
  }
  
  // TODO: clean up these hacks
  implicit object voidT      extends SqlNativeType [Unit]         (-1, (_,_) => sys.error("internal2"), (_,_,_) => sys.error("internal3"))
  implicit def setT[X:ast.SqlParamType] = new SqlNativeType [Set[X]]       (-1, (_,_) => sys.error("internal"), (_,_,_) => sys.error("internal")) {}
 
  // TODO: handle params
  implicit def toParser [X:SqlType] (c: ast.SqlNamedReqExpr[X]) = new ExprParser(c.name, implicitly[SqlType[X]], List(new query.SqlFragmentS(c.sql, c.params)))
  implicit def toParser2 [X:SqlType] (c: ast.SqlNonNullableCol[X]) = new ExprParser(c.name, implicitly[SqlType[X]], List(c.sql + " as " + c.name))
  implicit def toOptParser [X:SqlType] (c: ast.SqlNamedOptExpr[X]) = new OptionalExprParser(c.name, implicitly[SqlType[X]], List(new query.SqlFragmentS(c.sql, c.params)))
  implicit def toOptParser2 [X:SqlType] (c: ast.SqlNullableCol[X]) = new OptionalExprParser(c.name, implicitly[SqlType[X]], List(c.sql + " as " + c.name))
  
  private[scoop] def renderParams (params: Seq[SqlParam[_]]) = params.map(x => x.v + ":"+x.v.asInstanceOf[AnyRef].getClass.getName.stripPrefix("java.lang."))
}

private [scoop] sealed trait ParseResult[+T] {
  def map [X](fn: T => X): ParseResult[X] = this match {
    case ParseSuccess(v) => ParseSuccess(fn(v))
    case ParseFailure(e) => ParseFailure(e)
  }
  def flatMap [X](fn: T => ParseResult[X]): ParseResult[X] = fold (
    e => ParseFailure(e),
    v => fn(v)
  )
  def fold [X](lf: String => X, rf: T => X) = this match {
    case ParseFailure(e) => lf(e)
    case ParseSuccess(v) => rf(v)
  }
  def get = fold (
    e => sys.error(e),
    v => v
  )
}
private [scoop] case class ParseSuccess [T] (v: T) extends ParseResult[T]
private [scoop] case class ParseFailure (error: String) extends ParseResult[Nothing]

class ExprParser [+T] (name: String, exp: ast.SqlMappedType[T], sql: List[query.SqlFragmentS]) // sql should probably be Option
    extends boilerplate.ParserBase[T] (rs => exp.parse(rs, name) map {ParseSuccess(_)} getOrElse ParseFailure("Could not parse expression: " + name + " [" + exp + "] from " + util.inspectRS(rs))) {
  def columns = sql map (x => x: query.SelectExprS)
}

class OptionalExprParser [+T] (name: String, exp: ast.SqlMappedType[T], sql: List[query.SqlFragmentS]) 
    extends boilerplate.ParserBase[Option[T]] (rs => Some(exp.parse(rs, name)) map {ParseSuccess(_)} getOrElse ParseFailure("Could not parse expression: " + name + " [" + exp + "] from " + util.inspectRS(rs))) {
  def columns = sql map (x => x: query.SelectExprS)
}

sealed trait SqlParam [T] {
  val v: T
  def apply (stmt: PreparedStatement, idx: Int): Unit
}

case class SqlSingleParam [T,S] (v: T)(implicit val tp: ast.SqlParamType[T]) extends SqlParam[T] {
  def apply (stmt: PreparedStatement, idx: Int) = tp.set(stmt, idx, v)
}
case class SqlSetParam [T](v: Set[T])(implicit tp: ast.SqlParamType[T]) extends SqlParam[Set[T]] {
  def toList = v.toList.map(x => SqlSingleParam(x))
  def apply (stmt: PreparedStatement, idx: Int) = sys.error("WTF!")
}

trait ResultSetParser[+T] extends (ResultSet => ParseResult[T]) {self =>
  def map [X] (fn: T => X): ResultSetParser[X] = new ResultSetParser [X] {
    def apply (rs: ResultSet) = self(rs) map fn
    def columns = self.columns
    override def toString = "ResultSetParser(fn=" + util.fnToString(fn) + ")"
  }
  /* WARNING: resulting parser won't accumulate columns */
  def flatMap [X] (fn: T => ResultSetParser[X]): ResultSetParser[X] = new ResultSetParser [X] {
    def apply (rs: ResultSet) = for (x <- self(rs); y <- fn(x)(rs)) yield y
    def columns = self.columns 
    override def toString = "ResultSetParser(fn=" + util.fnToString(fn) + ")"
  }
  def columns: List[query.SelectExprS]
}

case class literal [T] (value: T) extends ResultSetParser [T] {
  def columns = Nil
  def apply (rs: ResultSet) = ParseSuccess(value)
}

