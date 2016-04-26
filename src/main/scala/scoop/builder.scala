package com.gravitydev.scoop
package builder

import com.gravitydev.scoop.query.{ParameterizedSql, stringToFragment, executeQuery}
import com.gravitydev.scoop.util.QueryResult
import java.sql.{Connection, ResultSet}

object `package` {
  type Query[T] = ast.Query[T]

  /** Clean up null assignments because of Option[SqlAssignment[_]] */
  def toAstSqlAssignments (assignments: Seq[Assignment]): Seq[ast.SqlAssignment] = assignments.filter(_ != null).map(_.underlying)
}

/** Wrapper for supporting optional setting of values */
private [scoop] final class Assignment (val underlying: ast.SqlAssignment)
object Assignment {
  implicit def fromSqlAssignment (a: ast.SqlAssignment): Assignment = new Assignment(a)
  implicit def fromOption (a: Option[ast.SqlAssignment]): Assignment = a.map(x => fromSqlAssignment(x)) getOrElse null
}

class JoinBuilder (queryable: ast.Queryable, predicate: ast.SqlExpr[Boolean]) {
  def build (joinType: ast.JoinType) = new ast.Join(queryable, predicate, joinType)
}

object InsertBuilder { 
  case class Into (into: ast.Table) {
    def set (assignments: Assignment*) = ast.Insert (into, toAstSqlAssignments(assignments))
    def values (assignments: Assignment*) = ast.Insert(into, toAstSqlAssignments(assignments))
    def apply (columns: ast.Column[_]*) = IntoColumns(into, columns.toList)
  }
  case class IntoColumns (into: ast.Table, columns: List[ast.Column[_]]) {
    def values (query: ast.Query[_]) = ast.InsertWithQuery(into, columns, query)
  }
}

object UpdateBuilder {
  case class Target(tb: ast.Table) {
    def set (assignments: Assignment*) = ast.Update (tb, toAstSqlAssignments(assignments), None)
  }
}

object DeleteBuilder {
  case class From (tb: ast.Table) {
    def where (pred: ast.SqlExpr[Boolean]) = new ast.Delete(tb, pred)
  }
}


trait QueryBuilderBase {
  private type S[X] = parsers.SelectionSingle[X]

  def select [A](a: S[A]) = buildQuery(a)
  def select [A,B](a: S[A], b: S[B]) = buildQuery(a~b)
  def select [A,B,C](a: S[A], b: S[B], c: S[C]) = buildQuery(a~b~c)
  def select [A,B,C,D](a: S[A], b: S[B], c: S[C], d: S[D]) = buildQuery(a~b~c~d)
  def select [A,B,C,D,E](a: S[A], b: S[B], c: S[C], d: S[D], e: S[E]) = buildQuery(a~b~c~d~e)

  protected def buildQuery[X](sel: Selection[X]): Query[X]
}

case class QueryBuilder(
  from:         Option[ast.Queryable],
  joins:        Seq[ast.Join]    = Nil,
  predicate:    Option[ast.SqlExpr[Boolean]]  = None,
  order:        Seq[ast.SqlOrdering]  = Nil,
  group:        Seq[ast.SqlExpr[_]]    = Nil,
  limit:        Option[Int]           = None,
  offset:       Option[Int]           = None,
  comment:      Option[String]        = None,
  distinct:     Boolean               = false,
  forUpdateLock: Boolean              = false
) extends QueryBuilderBase {
  def find [B](selection: Selection[B])(implicit c: Connection, dialect: SqlDialect): QueryResult[B] = buildQuery(selection) process selection
  def findDistinct [B](selection: Selection[B])(implicit c: Connection, dialect: SqlDialect): QueryResult[B] = buildQuery(selection).distinct process selection

  def innerJoin (join: JoinBuilder) = copy(joins = joins ++ List(join.build(ast.JoinType.Inner)))
  def leftJoin (join: JoinBuilder)  = copy(joins = joins ++ List(join.build(ast.JoinType.Left)))

  // always append? we'll go with that for now
  def where (pred: ast.SqlExpr[Boolean]) = copy(predicate = predicate.map(_ && pred).orElse(Some(pred)))

  def orderBy (order: ast.SqlOrdering*) = copy(order = this.order ++ order)
  def groupBy (cols: ast.SqlExpr[_]*) = copy(group = cols)
  def limit (l: Int): QueryBuilder = copy(limit = Some(l))
  def offset (o: Int): QueryBuilder = copy(offset = Some(o))
  def comment (c: String) = copy(comment = Some(c))
  
  protected def buildQuery[X](sel: Selection[X]): ast.Query[X] = ast.Query(sel, from, joins, predicate, order, group, limit, offset, distinct, forUpdateLock)
}

