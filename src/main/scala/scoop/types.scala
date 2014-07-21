package com.gravitydev.scoop
package ast

import java.sql.{ResultSet, PreparedStatement}

/**
 * Type that is used in the database
 */
trait SqlUnderlyingType[T]

/**
 * Type that is used in scala code
 */
trait SqlType[T] {
  def tpe: Int

  def parse (rs: ResultSet, name: String): Option[T]
  def parse (rs: ResultSet, index: Int): Option[T]

  // convenience
  def parseOr (rs: ResultSet, name: String, error: String): ParseResult[T] = parse(rs, name) map (x => Right(x)) getOrElse Left(error)
  def parseOr (rs: ResultSet, index: Int, error: String): ParseResult[T] = parse(rs, index) map (x => Right(x)) getOrElse Left(error) 

  def set (stmt: PreparedStatement, idx: Int, value: T): Unit

  def apply (name: String) = new ExprParser(name)(this)
}
object SqlType {
  @inline def apply [T](implicit t: SqlType[T]) = t
}
 
class SqlNativeType[T] (
  val tpe: Int, 
  getByName: (ResultSet, String) => T,
  getByIndex: (ResultSet, Int) => T,
  _set: (PreparedStatement, Int, T) => Unit
) extends SqlType [T] with SqlUnderlyingType[T] {
  def set (stmt: PreparedStatement, idx: Int, value: T): Unit = {
    if (value==null) stmt.setNull(idx, tpe)
    else _set(stmt, idx, value)
  }
  def parse (rs: ResultSet, name: String) = Option(getByName(rs, name)) filter {_ => !rs.wasNull}
  def parse (rs: ResultSet, idx: Int) = Option(getByIndex(rs, idx)) filter {_ => !rs.wasNull}
}

@deprecated("Use customType[T,N](...) istead", "1.0.0-alpha10")
class SqlCustomType[T,N] (from: N => T, to: T => N)(implicit nt: SqlNativeType[N]) extends SqlType[T] with SqlUnderlyingType[N] {
  def tpe = nt.tpe
  def parse (rs: ResultSet, name: String) = nt.parse(rs, name) map from
  def parse (rs: ResultSet, idx: Int) = nt.parse(rs, idx) map from
  def set (stmt: PreparedStatement, idx: Int, value: T): Unit = nt.set(stmt, idx, to(value))
}

class SqlWrappedType[T,C,N](from: C=>T, to: T=>C)(implicit ev: SqlType[C] with SqlUnderlyingType[N]) extends SqlType[T] with SqlUnderlyingType[N] {
  def tpe = ev.tpe
  def parse (rs: ResultSet, name: String) = ev.parse(rs, name) map from
  def parse (rs: ResultSet, idx: Int) = ev.parse(rs, idx) map from
  def set (stmt: PreparedStatement, idx: Int, value: T): Unit = ev.set(stmt, idx, to(value))
}

