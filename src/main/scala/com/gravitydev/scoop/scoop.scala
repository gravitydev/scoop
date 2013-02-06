package com.gravitydev.scoop

import java.sql.{ResultSet, PreparedStatement, Types, Timestamp, Date}
import util.Logging

object `package` {
  type Table[T <: ast.SqlTable[T]] = ast.SqlTable[T]
  type TableCompanion[T <: Table[T]] = {def apply() : T}

  def opt [T](p: ResultSetParser[T]): boilerplate.ParserBase[Option[T]] = {
    new boilerplate.ParserBase [Option[T]] (rs => p(rs) match {
      case ParseSuccess(s) => ParseSuccess(Option(s))
      case ParseFailure(e) => ParseSuccess(None)
    }) {
      def columns = p.columns
    }
  }

  implicit object int        extends SqlNativeType [Int]          (Types.INTEGER,   _ getInt _,       _ setInt (_,_))  
  implicit object long       extends SqlNativeType [Long]         (Types.BIGINT,    _ getLong _,      _ setLong (_,_))
  implicit object double     extends SqlNativeType [Double]       (Types.DOUBLE,    _ getDouble _,    _ setDouble (_,_))
  implicit object string     extends SqlNativeType [String]       (Types.VARCHAR,   _ getString _,    _ setString (_,_))
  implicit object timestamp  extends SqlNativeType [Timestamp]    (Types.TIMESTAMP, _ getTimestamp _, _ setTimestamp (_,_))
  implicit object date       extends SqlNativeType [Date]         (Types.DATE,      _ getDate _,      _ setDate (_,_))
  implicit object boolean    extends SqlNativeType [Boolean]      (Types.BOOLEAN,   _ getBoolean _,   _ setBoolean (_,_))
  implicit object decimal    extends SqlNativeType [scala.math.BigDecimal] (Types.DECIMAL, (rs, idx) => scala.math.BigDecimal(rs.getBigDecimal(idx)), (rs, idx, value) => rs.setBigDecimal(idx, value.underlying))
  
  implicit def toColumnParser [X](c: ast.SqlNonNullableCol[X]) = new ColumnParser(c)
  implicit def toNullableColumnParser [X](c: ast.SqlNullableCol[X]) = new NullableColumnParser(c)
  implicit def toColumnWrapper [X](c: ast.SqlCol[X]) = ColumnWrapper(c)
  
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

trait SqlType [T] {self =>
  def tpe: Int // jdbc sql type
  def set (stmt: PreparedStatement, idx: Int, value: T): Unit
  def parse (rs: ResultSet, name: String): Option[T]
  def apply (n: String, sql: String = "") = new ExprParser (n, this, sql)
}
  
abstract class SqlNativeType[T] (val tpe: Int, get: (ResultSet, String) => T, _set: (PreparedStatement, Int, T) => Unit) extends SqlType [T] with Logging {
  def set (stmt: PreparedStatement, idx: Int, value: T): Unit = {
    if (value==null) stmt.setNull(idx, tpe)
    else _set(stmt, idx, value)
  }
  def parse (rs: ResultSet, name: String) = Option(get(rs, name)) filter {_ => !rs.wasNull}
}
abstract class SqlCustomType[T,N] (from: N => T, to: T => N)(implicit nt: SqlNativeType[N]) extends SqlType[T] {
  def tpe = nt.tpe
  def parse (rs: ResultSet, name: String) = nt.parse(rs, name) map from
  def set (stmt: PreparedStatement, idx: Int, value: T): Unit = nt.set(stmt, idx, to(value))
}

sealed trait SqlParam [T] {
  val v: T
  def apply (stmt: PreparedStatement, idx: Int): Unit
}

case class SqlSingleParam [T,S] (v: T)(implicit val tp: SqlType[T]) extends SqlParam[T] {
  def apply (stmt: PreparedStatement, idx: Int) = tp.set(stmt, idx, v)
}
case class SqlSetParam [T](v: Set[T])(implicit tp: SqlType[T]) extends SqlParam[Set[T]] {
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

class ExprParser [+T] (name: String, exp: SqlType[T], sql: String = "") 
    extends boilerplate.ParserBase[T] (rs => exp.parse(rs, name) map {ParseSuccess(_)} getOrElse ParseFailure("Could not parse expression: " + name + " [" + exp + "] from " + util.inspectRS(rs))) {
  def prefix (pf: String) = new ExprParser (pf+name, exp)
  def columns = List(sql) filter (_!="") map (x => x+" as "+name: query.SelectExprS)
}

class ColumnParser[T](column: ast.SqlNonNullableCol[T]) 
    extends boilerplate.ParserBase[T] (rs => 
      column parse rs map {ParseSuccess(_)} getOrElse {
        ParseFailure("Could not parse column [" + column._alias + "] from " + util.inspectRS(rs))
      }
    ) {
  def name = column.name
  def columns = List(column.selectSql)
  override def toString = "ColumnParser(column=" + column + ")"
}

class NullableColumnParser[T](column: ast.SqlNullableCol[T]) 
    extends boilerplate.ParserBase[Option[T]] (rs => 
      column parse rs map {ParseSuccess(_)} getOrElse {
        ParseFailure("Could not parse [" + column.name + "] (optional) from " + util.inspectRS(rs)) 
      }
    ) {
  def name = column.name
  def columns = List(column.selectSql)
  override def toString = "NullableColumnParser(column=" + column + ")"
}

abstract class SqlOrder (val sql: String)
case object Ascending   extends SqlOrder ("ASC")
case object Descending  extends SqlOrder ("DESC")

case class SqlOrdering (col: ast.SqlCol[_], order: SqlOrder) {
  def sql = col.sql + " " + order.sql
}

case class ColumnWrapper [X](col: ast.SqlCol[X]) {
  def desc  = SqlOrdering(col, Descending)
  def asc   = SqlOrdering(col, Ascending)
}

