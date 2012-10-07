package com.gravitydev.scoop

import java.sql.ResultSet

object `package` {
  type TableCompanion[T] = {def apply(s: String): T}
  
  private def getPrimitive [T](rs: ResultSet)(fn: ResultSet => T) = Option(fn(rs)) filter (_ => !rs.wasNull)

  def opt [T](p: Parser[T]): Parser[Option[T]] = ParserWrapper(p, (opt: Option[T]) => Option(opt)) 

  implicit object SqlInt      extends SqlBasicType  [Int]     (_ getInt _)  
  implicit object SqlLong     extends SqlBasicType  [Long]    (_ getLong _)
  implicit object SqlString   extends SqlBasicType  [String]  (_ getString _)
  //implicit object SqlBigDecimal extends SqlType     [java.math.BigDecimal, BigDecimal]  (_ getBigDecimal _, x => x, x => x)
  
  implicit def toColumnParser [X](c: Col[X]) = ColumnParser(c)
  implicit def toColumnWrapper [X](c: Col[X]) = ColumnWrapper(c)
  implicit def exprListToParser (exprList: strong.SqlExprList) = new SqlExprListParser(exprList)
}

class SqlExprListParser (sel: strong.SqlExprList) {
  /*def parse (rs: ResultSet) = {
    "test"
  }*/
}


class SqlType[T,S](extract: (ResultSet, String) => S, from: S => T, to: T => S) {
  def get (name: String)(implicit rs: ResultSet) = {
    Option(extract(rs, name)) filter {_ => rs.wasNull} map from
  }
}

class SqlBasicType [T] (extract: (ResultSet, String) => T) extends SqlType[T,T](extract, x=>x, x=>x)


class Col[T](val name: String)(implicit val table: Table[_], sqlType: SqlType[T,_]) {
  def parse (rs: ResultSet, alias: String = null) = sqlType.get(Option(alias) getOrElse name)(rs)
  override def toString = "Col("+name+")"
  def sql = table.as + "." + name
}

case class JoinParser [L,R] (l: Parser[L], r: Parser[R]) extends Parser [L~R] {
  def parse (rs: ResultSet) = for {
    x <- l parse rs
    y <- r parse rs
  } yield new ~ (x,y)
  
  def as (table: String = null, prefix: String = null) = l.as(table,prefix) ~ r.as(table,prefix)
  def columns = l.columns ++ r.columns
}

case class ~ [L,R] (l: L, r: R)
  
case class ParserWrapper [T,X] (p: Parser[T], fn: Option[T] => Option[X]) extends Parser[X] {
  def columns = p.columns
  def as (table: String = null, prefix: String = null) = ParserWrapper(p.as(table, prefix), fn)
  def parse (rs: ResultSet) = fn(p.parse(rs))
}

trait Parser[T] extends (ResultSet => T) {self =>
  def apply (rs: ResultSet): T = parse(rs).get
  def parse (rs: ResultSet): Option[T]
  def ~ [X](p: Parser[X]) = JoinParser(this, p)
  def as (table: String = null, prefix: String = null): Parser[T]
  def columns: List[String]
  def map [X](fn: T => X): Parser[X] = new Parser [X] {
    def columns = self.columns
    def as (table: String = null, prefix: String = null) = self.as(table, prefix) map fn
    def parse (rs: ResultSet) = self.parse(rs) map fn
  }
}

case class ColumnParser[T](column: Col[T], tableAlias: Option[String] = None, pf: String = "") extends Parser[T] {
  def parse (rs: ResultSet) = column.parse(rs, pf+column.name)
  def as (table: String = null, prefix: String = null) = new ColumnParser(column, Option(table), Option(prefix) getOrElse "")
  def columns = List((tableAlias map {_+"."} getOrElse "") + column.name + (if (pf != "") " as " + pf + column.name else ""))
}

abstract class Table [T <: Table[T]](_tableName: String, companion: TableCompanion[T]) extends strong.Queryable[T] {
  val tableName = Option(_tableName) getOrElse companion.getClass.getCanonicalName.split('.').last.stripSuffix("$")
  def as: String
  implicit def _self = this
  def col[T](name: String)(implicit st: SqlType[T,_]) = new Col[T](name)
  def col[T](name: Symbol)(implicit st: SqlType[T,_]) = new Col[T](name.name)
  def as (alias: String): T = companion(alias)
  def sql = tableName + " as " + as
}

abstract class SqlOrder (val sql: String)
case object Ascending   extends SqlOrder ("ASC")
case object Descending  extends SqlOrder ("DESC")

case class SqlOrdering (col: Col[_], order: SqlOrder) {
  def sql = col.sql + " " + order.sql
}

case class ColumnWrapper [X](col: Col[X]) {
  def desc  = SqlOrdering(col, Descending)
  def asc   = SqlOrdering(col, Ascending)
}
