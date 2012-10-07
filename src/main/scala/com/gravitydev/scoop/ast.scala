package com.gravitydev.scoop
package ast

sealed trait Sql {
  def sql: String
}

sealed trait SqlExpr [T] extends Sql

sealed trait SqlPredicate extends Sql {
  def sql: String
}

sealed abstract class SqlBooleanOp (op: String) extends Sql {
  def sql = op
}
object SqlEqualsOp extends SqlBooleanOp("=")
object SqlLessThanOp extends SqlBooleanOp("<")
object SqlGreaterThanOp extends SqlBooleanOp(">")

case class SqlComparisonExpr [L,R](l: SqlValueExpr[L], r: SqlValueExpr[R], op: SqlBooleanOp) extends SqlPredicate {
  def sql = l.sql + " " + op.sql + " " + r.sql
}

sealed trait SqlValueExpr [X] extends Sql {
  def === [T <% X] (v: SqlValueExpr[T]) = SqlComparisonExpr(this, v, SqlEqualsOp)
  // alias
  def |=| [T <% X] (v: SqlValueExpr[T]) = === (v)
  
  
  def < [T <% X] (v: SqlValueExpr[T]) = SqlComparisonExpr(this, v, SqlLessThanOp)
  def > [T <% X] (v: SqlValueExpr[T]) = SqlComparisonExpr(this, v, SqlGreaterThanOp)
}

case class SqlLiteralExpr [T] (v: T) extends SqlValueExpr[T] {
  def sql = v.toString
}

case class ColExpr [T](col: Col[T]) extends SqlExpr[T] {
  def sql = col.sql
}
