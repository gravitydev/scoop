package com.gravitydev.scoop
package functions

import query._

object `package` {
  def count (expr: ast.SqlExpr[_]) = sql[Int]("COUNT(" +~ expr +~ ")")
  def avg [T:SqlType](expr: ast.SqlExpr[T]) = sql[T]("AVG(" +~ expr +~ ")")
  def max [T:SqlType](expr: ast.SqlExpr[T]) = sql[T]("MAX(" +~ expr +~ ")")
  def min [T:SqlType](expr: ast.SqlExpr[T]) = sql[T]("MIN(" +~ expr +~ ")")
  val now = sql[DateTime]("NOW()")
}

