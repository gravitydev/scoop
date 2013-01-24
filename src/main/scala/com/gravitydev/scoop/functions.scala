package com.gravitydev.scoop
package functions

import query._

object `package` {
  def avg [T](expr: ast.SqlExpr[T]) = sql[T]("AVG(" +~ expr +~ ")")
  def count (expr: ast.SqlExpr[_]) = sql[Int]("COUNT(" +~ expr +~ ")")
}
