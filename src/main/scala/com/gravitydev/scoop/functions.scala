package com.gravitydev.scoop
package functions

import query._

object `package` {
  def count (expr: ast.SqlExpr[_]) = sql[Long]("COUNT(" +~ expr +~ ")")
  def countDistinct (expr: ast.SqlExpr[_]) = sql[Int]("COUNT(DISTINCT " +~ expr +~ ")")
  def sum (expr: ast.SqlExpr[_]) = sql[Long]("SUM(" +~ expr +~ ")")
  def avg [T:SqlType](expr: ast.SqlExpr[T]) = sql[T]("AVG(" +~ expr +~ ")")
  def max [T:SqlType](expr: ast.SqlExpr[T]) = sql[T]("MAX(" +~ expr +~ ")")
  def min [T:SqlType](expr: ast.SqlExpr[T]) = sql[T]("MIN(" +~ expr +~ ")")
  val now = sql[java.sql.Timestamp]("NOW()")


  // why doesn't the implicit pick up SqlFragmentS.fromExpr
  def coalesce [T:SqlType](exprs: ast.SqlExpr[T]*) = 
    query.sql[T]("COALESCE(" +~ exprs.map(SqlFragmentS.fromExpr _).reduceLeft(_ +~ ", " +~ _) +~ ")")
}

