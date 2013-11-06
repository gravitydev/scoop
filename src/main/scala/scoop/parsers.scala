package com.gravitydev.scoop
package parsers

import ast.SqlResultType
import java.sql.ResultSet

// TODO: use either? try?
private [scoop] sealed trait ParseResult[+T] {
  def map [X](fn: T => X): ParseResult[X] = this match {
    case ParseSuccess(v) => ParseSuccess(fn(v))
    case ParseFailure(e) => ParseFailure(e)
  }
  def flatMap [X](fn: T => ParseResult[X]): ParseResult[X] = fold (
    e => ParseFailure(e),
    v => fn(v)
  )
  def fold [X](lf: String => X, rf: T => X) = this match {
    case ParseFailure(e) => lf(e)
    case ParseSuccess(v) => rf(v)
  }
  def get = fold (
    e => sys.error(e),
    v => v
  )
}
private [scoop] case class ParseSuccess [T] (v: T) extends ParseResult[T]
private [scoop] case class ParseFailure (error: String) extends ParseResult[Nothing]

// TODO: sql should probably be Option
class ExprParser [+T:SqlResultType] (name: String, sql: List[query.SqlFragmentS]) extends ParserBase[T] (
  rs => implicitly[SqlResultType[T]].parse(rs, name) map {ParseSuccess(_)} getOrElse ParseFailure("Could not parse expression: " + name + " [" + implicitly[SqlResultType[T]] + "] from " + util.inspectRS(rs))
) {
  def columns = sql map (x => x: query.SelectExprS)
}

class OptionalExprParser [+T:SqlResultType] (name: String, sql: List[query.SqlFragmentS]) extends ParserBase[Option[T]] (
  rs => Some(implicitly[SqlResultType[T]].parse(rs, name)) map {ParseSuccess(_)} getOrElse ParseFailure("Could not parse expression: " + name + " [" + implicitly[SqlResultType[T]] + "] from " + util.inspectRS(rs))
) {
  def columns = sql map (x => x: query.SelectExprS)
}

trait ResultSetParser[+T] extends (ResultSet => ParseResult[T]) {self =>
  def map [X] (fn: T => X): ResultSetParser[X] = new ResultSetParser [X] {
    def apply (rs: ResultSet) = self(rs) map fn
    def columns = self.columns
    override def toString = "ResultSetParser(fn=" + util.fnToString(fn) + ")"
  }
  /* WARNING: resulting parser won't accumulate columns, breaks monad */
  def flatMap [X] (fn: T => ResultSetParser[X]): ResultSetParser[X] = new ResultSetParser [X] {
    def apply (rs: ResultSet) = for (x <- self(rs); y <- fn(x)(rs)) yield y
    def columns = self.columns 
    override def toString = "ResultSetParser(fn=" + util.fnToString(fn) + ")"
  }
  def columns: List[query.SelectExprS]
}

class ParserWrapper[+T](parser: ResultSetParser[T]) {
  def ~ [X](px: P[X]) = new Parser2(parser, px)
  def >> [X](fn: T=>X) = new Parser1(parser map fn)
}

