package com.gravitydev.scoop

import java.sql.ResultSet

object `package` {
  type Table[T <: ast.SqlTable[T]] = ast.SqlTable[T]
  type TableCompanion[T <: Table[T]] = {def apply(s: String): T}

  def opt [T](p: Parser[T]): Parser[Option[T]] = ParserWrapper(p, (opt: Option[T]) => Option(opt))

  implicit object SqlInt      extends SqlBasicType  [Int]     (_ getInt _)  
  implicit object SqlLong     extends SqlBasicType  [Long]    (_ getLong _)
  implicit object SqlString   extends SqlBasicType  [String]  (_ getString _)
  //implicit object SqlBigDecimal extends SqlType     [java.math.BigDecimal, BigDecimal]  (_ getBigDecimal _, x => x, x => x)
  
  implicit def toColumnParser [X](c: ast.SqlCol[X]) = ColumnParser(c)
  implicit def toColumnWrapper [X](c: ast.SqlCol[X]) = ColumnWrapper(c)
}

sealed trait SqlParam [T] {
  val v: T
}
case class SqlSingleParam [T] (v: T)(implicit tp: SqlType[T,_]) extends SqlParam[T]
case class SqlSetParam [T](v: Set[T])(implicit tp: SqlType[T,_]) extends SqlParam[Set[T]]

abstract class SqlType[T,S](extract: (ResultSet, String) => S, from: S => T, to: T => S) {
  def get (name: String)(implicit rs: ResultSet) = Option(extract(rs, name)) filter {_ => !rs.wasNull} map from
}

class SqlBasicType [T] (extract: (ResultSet, String) => T) extends SqlType[T,T](extract, x=>x, x=>x)

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
  def as (table: String = null, prefix: String = null): Parser[T]
  def columns: List[query.ExprS]
  def map [X](fn: T => X): Parser[X] = new Parser [X] {
    def columns = self.columns
    def as (table: String = null, prefix: String = null) = self.as(table, prefix) map fn
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
  
  def as (table: String = null, prefix: String = null) = l.as(table,prefix) ~ r.as(table,prefix)
  def columns = l.columns ++ r.columns
}

case class ~ [L,R] (l: L, r: R)
  
case class ParserWrapper [T,X] (p: Parser[T], fn: Option[T] => Option[X]) extends Parser[X] {
  def name = "ParserWrapper(" + p.name + ")"
  def columns = p.columns
  def as (table: String = null, prefix: String = null) = ParserWrapper(p.as(table, prefix), fn)
  def parse (rs: ResultSet) = fn(p.parse(rs))
}

case class ColumnParser[T](column: ast.SqlCol[T], tableAlias: Option[String] = None, pf: String = "") extends Parser[T] {
  def name = pf+column.name
  def parse (rs: ResultSet) = column.parse(rs, pf+column.name)
  def as (table: String = null, prefix: String = null) = new ColumnParser(column, Option(table), Option(prefix) getOrElse "")
  def columns = List((tableAlias map {_+"."+column.name} getOrElse column.sql) + (if (pf != "") " as " + pf + column.name else ""))
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
