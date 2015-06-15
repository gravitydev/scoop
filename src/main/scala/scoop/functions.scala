package com.gravitydev.scoop
package functions

import query._, ast.SqlType

object `package` {
  def count (expr: ast.SqlExpr[_]) = sqlExpr[Long]("COUNT(" +~ expr +~ ")")
  def countDistinct (expr: ast.SqlExpr[_]) = sqlExpr[Long]("COUNT(DISTINCT " +~ expr +~ ")")
  def sum (expr: ast.SqlExpr[_]) = sqlExpr[Long]("SUM(" +~ expr +~ ")")
  def avg [T:SqlType](expr: ast.SqlExpr[T]) = sqlExpr[T]("AVG(" +~ expr +~ ")")
  def max [T:SqlType](expr: ast.SqlExpr[T]) = sqlExpr[T]("MAX(" +~ expr +~ ")")
  def min [T:SqlType](expr: ast.SqlExpr[T]) = sqlExpr[T]("MIN(" +~ expr +~ ")")
  val now = sqlExpr[java.sql.Timestamp]("NOW()")

  // why doesn't the implicit pick up SqlFragmentS.fromExpr
  def coalesce [T:SqlType](exprs: ast.SqlExpr[T]*) = 
    query.sqlExpr[T]("COALESCE(" +~ exprs.map(SqlFragmentS.fromExpr _).reduceLeft(_ +~ ", " +~ _) +~ ")")
}

