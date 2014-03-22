package com.gravitydev.scoop
package ast

import java.sql.{ResultSet, PreparedStatement}

trait SqlType[T] {
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
) extends SqlType [T] {
  def set (stmt: PreparedStatement, idx: Int, value: T): Unit = {
    if (value==null) stmt.setNull(idx, tpe)
    else _set(stmt, idx, value)
  }
  def parse (rs: ResultSet, name: String) = Option(getByName(rs, name)) filter {_ => !rs.wasNull}
  def parse (rs: ResultSet, idx: Int) = Option(getByIndex(rs, idx)) filter {_ => !rs.wasNull}
}

class SqlCustomType[T,N] (from: N => T, to: T => N)(implicit nt: SqlNativeType[N]) extends SqlType[T] {
  def tpe = nt.tpe
  def parse (rs: ResultSet, name: String) = nt.parse(rs, name) map from
  def parse (rs: ResultSet, idx: Int) = nt.parse(rs, idx) map from
  def set (stmt: PreparedStatement, idx: Int, value: T): Unit = nt.set(stmt, idx, to(value))
}

