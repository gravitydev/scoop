package com.gravitydev.scoop

import java.sql.{ResultSet, PreparedStatement, Types, Timestamp}

object `package` {
  type Table[T <: ast.SqlTable[T]] = ast.SqlTable[T]
  type TableCompanion[T <: Table[T]] = {def apply(s: String): T}

  def opt [T](p: Parser[T]): Parser[Option[T]] = ParserWrapper(p, (opt: Option[T]) => Option(opt))

  implicit object SqlInt        extends SqlNativeType [Int]       (Types.INTEGER,   _ getInt _,       _ setInt (_,_))  
  implicit object SqlLong       extends SqlNativeType [Long]      (Types.BIGINT,    _ getLong _,      _ setLong (_,_))
  implicit object SqlString     extends SqlNativeType [String]    (Types.VARCHAR,   _ getString _,    _ setString (_,_))
  implicit object SqlTimestamp  extends SqlNativeType [Timestamp] (Types.TIMESTAMP, _ getTimestamp _, _ setTimestamp (_,_))
  
  implicit def toColumnParser [X](c: ast.SqlCol[X]) = ColumnParser(c)
  implicit def toNullableColumnParser [X](c: ast.SqlNullableCol[X]) = NullableColumnParser(c)
  implicit def toColumnWrapper [X](c: ast.SqlCol[X]) = ColumnWrapper(c)
  
  private[scoop] def renderParams (params: Seq[SqlSingleParam[_,_]]) = params.map(x => x.v + ":"+x.v.asInstanceOf[AnyRef].getClass.getName.stripPrefix("java.lang."))
}

trait SqlType [T] {
  def tpe: Int // jdbc sql type
  def apply (stmt: PreparedStatement, idx: Int, value: T): Unit
  def parse (rs: ResultSet, name: String): Option[T]
}
  
abstract class SqlNativeType[T] (val tpe: Int, get: (ResultSet, String) => T, set: (PreparedStatement, Int, T) => Unit) extends SqlType [T] {
  def apply (stmt: PreparedStatement, idx: Int, value: T): Unit = set(stmt, idx, value)
  def parse (rs: ResultSet, name: String) = Option(get(rs, name)) filter {_ => !rs.wasNull}
}
abstract class SqlCustomType[T,N] (from: N => T, to: T => N)(implicit nt: SqlNativeType[N]) extends SqlType[T] {
  def tpe = nt.tpe
  def parse (rs: ResultSet, name: String) = nt.parse(rs, name) map from
  def apply (stmt: PreparedStatement, idx: Int, value: T): Unit = nt.apply(stmt, idx, to(value))
}

sealed trait SqlParam [T] {
  val v: T
}

case class SqlSingleParam [T,S] (v: T)(implicit val tp: SqlType[T]) extends SqlParam[T] {
  def apply (stmt: PreparedStatement, idx: Int) = tp.apply(stmt, idx, v)
}
case class SqlSetParam [T](v: Set[T])(implicit tp: SqlType[T]) extends SqlParam[Set[T]] {
  def toList = v.toList.map(x => SqlSingleParam(x))
}

trait Parser[T] extends (ResultSet => T) {self =>
  private def resultSetInfo (rs: ResultSet) = {
    val md = rs.getMetaData
    val columnCount = md.getColumnCount
    
    val colData = for (i <- 1 to columnCount) yield "Column(column=" + md.getTableName(i)+"."+md.getColumnName(i) + ", label=" + md.getColumnLabel(i) + ", type=" + md.getColumnTypeName(i) + ")"
    
    colData.mkString("ResultSet(", ", ", ")")
  }
  def name: String
  def apply (rs: ResultSet): T = parse(rs) getOrElse error("Could not parse result set: " + resultSetInfo(rs) + " with parser: " + self.name)
  def parse (rs: ResultSet): Option[T]
  def ~ [X](p: Parser[X]) = JoinParser(this, p)
  def as (prefix: String = null): Parser[T]
  def columns: List[query.ExprS]
  def map [X](fn: T => X): Parser[X] = new Parser [X] {
    def columns = self.columns
    def as (prefix: String = null) = self.as(prefix) map fn
    def parse (rs: ResultSet) = self.parse(rs) map fn
    def name = "Parser(" + self.name + ")"
  }
}

case class JoinParser [L,R] (l: Parser[L], r: Parser[R]) extends Parser [L~R] {
  def name = l.name + " ~ " + r.name
  def parse (rs: ResultSet) = for {
    x <- l parse rs
    y <- r parse rs
  } yield new ~ (x,y)
  
  def as (prefix: String = null) = l.as(prefix) ~ r.as(prefix)
  def columns = l.columns ++ r.columns
}

case class ~ [L,R] (l: L, r: R)
  
case class ParserWrapper [T,X] (p: Parser[T], fn: Option[T] => Option[X]) extends Parser[X] {
  def name = "ParserWrapper(" + p.name + ")"
  def columns = p.columns
  def as (prefix: String = null) = ParserWrapper(p.as(prefix), fn)
  def parse (rs: ResultSet) = fn(p.parse(rs))
}

case class ColumnParser[T](column: ast.SqlCol[T], pf: String = "") extends Parser[T] {
  def name = pf+column.name
  def parse (rs: ResultSet) = column.parse(rs, pf+column.name)
  def as (prefix: String = null) = new ColumnParser(column, Option(prefix) getOrElse "")
  def columns = List(column.sql + (if (pf != "") " as " + pf + column.name else ""))
}

case class NullableColumnParser[T](column: ast.SqlNullableCol[T], pf: String = "") extends Parser[Option[T]] {
  def name = pf+column.name
  def parse (rs: ResultSet) = column.parse(rs, pf+column.name)
  def as (prefix: String = null) = new NullableColumnParser(column, Option(prefix) getOrElse "")
  def columns = List(column.sql + (if (pf != "") " as " + pf + column.name else ""))
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
