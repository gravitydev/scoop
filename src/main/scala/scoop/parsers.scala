package com.gravitydev.scoop
package parsers

import ast.SqlResultType
import query.SelectExprS
import java.sql.ResultSet

object `package` {
  private [parsers] type P[+T] = ResultSetParser[T]
  private [parsers] type S[+T] = Selection[T]
 
  implicit def rightBiasParseResult [T](r: ParseResult[T]): Either.RightProjection[String,T] = r.right

  type ResultSetParser[+T] = ResultSet => ParseResult[T]
}

// Convenience to be able to flatMap a parser in the boiler plate code
private [scoop] trait ResultSetParserFlatMap [+T] {self: (ResultSet => ParseResult[T]) =>
  def flatMap [X] (fn: T => ResultSetParser[X]): ResultSetParser[X] = (rs: ResultSet) => (apply(rs).right flatMap (x => fn(x)(rs)))
}

final private [parsers] class ResultSetParserImpl[+T] (impl: ResultSet => ParseResult[T]) extends ResultSetParser[T] {
  @inline def apply (rs: ResultSet): ParseResult[T] = impl(rs)
}

private [scoop] class MappedSelection [T,+X] (parser: Selection[T], fn: T => X) extends Selection[X] {
  def apply (rs: ResultSet) = parser(rs).right map fn
  lazy val expressions = parser.expressions
  override def toString = "Selection(expressions=" + expressions + ", fn=" + util.fnToString(fn) + ")"
}

class ExprParser [+T:SqlResultType] (name: String) extends ParserBase[T] (
  rs => SqlResultType[T].parse(rs, name) map {Right(_)} getOrElse Left("Could not parse expression: " + name + " [" + SqlResultType[T] + "] from " + util.inspectRS(rs))
) {
  // TODO: i don't like this "columns" method on a parser that is just a parser
  def columns = Nil
}

class OptionalExprParser [+T:SqlResultType] (name: String, sql: List[query.SqlFragmentS]) extends ParserBase[Option[T]] (
  rs => Some(SqlResultType[T].parse(rs, name)) map {Right(_)} getOrElse Left("Could not parse expression: " + name + " [" + SqlResultType[T] + "] from " + util.inspectRS(rs))
) {
  def columns = sql map (x => x: query.SelectExprS)
}

