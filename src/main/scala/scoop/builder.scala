package com.gravitydev.scoop
package builder

import com.gravitydev.scoop.query.{SqlS, QueryS, stringToFragment, executeQuery}
import com.gravitydev.scoop.util.QueryResult
import java.sql.{Connection, ResultSet}

object `package` {
  // convenience
  type TableT = ast.SqlTable[T] forSome {type T <: ast.SqlTable[T]}

  /** Clean up null assignments because of Option[SqlAssignment[_]] */
  def toAstSqlAssignments (assignments: Seq[Assignment]): Seq[ast.SqlAssignment[_]] = assignments.filter(_ != null).map(_.underlying)
}

/** Wrapper for supporting optional setting of values */
sealed class Assignment (val underlying: ast.SqlAssignment[_]) 
object Assignment {
  implicit def fromSqlAssignment [T](a: ast.SqlAssignment[T]): Assignment = new Assignment(a)
  implicit def fromOption [T](a: Option[ast.SqlAssignment[T]]): Assignment = a.map(x => fromSqlAssignment(x)) getOrElse null
}

class From (queryable: String, val params: List[SqlParam[_]] = Nil) {
  val sql = "FROM " + queryable
}
object From {
  implicit def fromTable (t: ast.SqlTable[_]) = new From(t.sql)
  implicit def fromNamedQuery (q: ast.SqlNamedQuery) = new From(q.sql, q.params)
  implicit def fromAliasedSql (q: query.AliasedSqlFragmentS) = new From(q.sql, q.params)
}

class JoinBuilder (queryable: query.SqlFragmentS, predicate: ast.SqlExpr[Boolean]) {
  def build (joinType: JoinType) = new Join(queryable, predicate, joinType)
}

class Join (queryable: query.SqlFragmentS, predicate: ast.SqlExpr[Boolean], joinType: JoinType) {
  def sql: String = joinType.sql + " JOIN " + queryable.sql + " ON " + predicate.sql
  def params = queryable.params ++ predicate.params
}
sealed abstract class JoinType (val sql: String)
object JoinType {
  case object Inner extends JoinType("INNER")
  case object Left extends JoinType("LEFT")
}

sealed abstract class SqlOrder (val sql: String)
object SqlOrder {
  case object Ascending   extends SqlOrder ("ASC")
  case object Descending  extends SqlOrder ("DESC")
}
class SqlOrdering (expr: ast.SqlExpr[_], order: SqlOrder) {
  def sql = expr.sql + " " + order.sql
  def params = expr.params
}
object SqlOrdering {
  implicit def fromExpr (expr: ast.SqlExpr[_]): SqlOrdering = expr.asc
}


class InsertBuilder (into: String) {
  def set (assignments: Assignment*) = Insert (into, toAstSqlAssignments(assignments))
  def values (assignments: Assignment*) = Insert2(into, toAstSqlAssignments(assignments))
  def apply (columns: ast.SqlCol[_]*) = new InsertBuilder2(into, columns.toList)
}
class InsertBuilder2 (into: String, columns: List[ast.SqlCol[_]]) {
  def values (sql: SqlS) = Insert3(into, columns, sql)
}

class UpdateBuilder (tb: TableT) {
  def set (assignments: Assignment*) = Update (tb, toAstSqlAssignments(assignments), None)
}

class DeleteBuilder (tb: TableT) {
  def where (pred: ast.SqlExpr[Boolean]) = new Delete(tb, pred)
}

sealed trait InsertBase {
  def params: Seq[SqlParam[_]]
  def sql: String
  def apply ()(implicit c: Connection) = try {
    import java.sql.Statement
    util.using(c.prepareStatement(sql, Statement.RETURN_GENERATED_KEYS)) {stmt => 
      for ((p, idx) <- params.zipWithIndex) p(stmt, idx+1)
      stmt.executeUpdate()

      // TODO: this is a hack, do this right
      try util.using(stmt.getGeneratedKeys()) {rs =>
        rs.next()
        Some(rs.getLong(1))
      } catch {
        case _:Throwable => None
      }
    }
  } catch {
    case e: java.sql.SQLException => throw new Exception("SQL Exception ["+e.getMessage+"] when executing query ["+sql+"] with parameters: ["+params+"]")
  }

  // upsert
  def onDuplicateKeyUpdate (assignments: Assignment*) = Upsert(this, toAstSqlAssignments(assignments))
}

case class Insert (
  into: String,
  assignments: Seq[ast.SqlAssignment[_]] = Nil,
  comment: Option[String] = None
) extends InsertBase {
  def comment (c: String): Insert = copy(comment = Some(c))
  def sql: String = "INSERT INTO " + into + " SET " + assignments.map(_.sql).mkString(", ")
  def params = assignments.flatMap(_.params)
}

// standard syntax
case class Insert2 (
  into: String,
  assignments: Seq[ast.SqlAssignment[_]],
  comment: Option[String] = None
) extends InsertBase {
  def comment (c: String): Insert2 = copy(comment = Some(c))
  def params: List[SqlParam[_]] = assignments.foldLeft(List[SqlParam[_]]()){(a,b) => a ++ b.params}
  def sql: String = "INSERT INTO " + into + " (" + assignments.map(_.col.columnName).mkString(", ") + ") VALUES (" + assignments.map(_.valueSql).mkString(", ") + ")"
}

case class InsertBatch (
  into: String,
  values: List[List[String]],
  comment: Option[String] = None
)

case class Upsert (
  insert: InsertBase,
  assignments: Seq[ast.SqlAssignment[_]] = Nil
) extends InsertBase {
  def sql = insert.sql + " ON DUPLICATE KEY UPDATE " + assignments.map(_.sql).mkString(", ")
  def params = insert.params ++ assignments.flatMap(_.params)
}

// using a subselect
case class Insert3 (
  into: String,
  columns: List[ast.SqlCol[_]],
  query: SqlS,
  comment: Option[String] = None
) extends InsertBase {
  def comment (c: String): Insert3 = copy(comment = Some(c))
  def params: List[SqlParam[_]] = query.params 
  def sql: String = "INSERT INTO " + into + " (" + columns.map(_.columnName).mkString(", ") + ")\n" + query.sql
}

case class Update (
  table: TableT,
  assignments: Seq[ast.SqlAssignment[_]] = Nil,
  predicate: Option[ast.SqlExpr[Boolean]] = None,
  comment: Option[String] = None
) {
  def where (pred: ast.SqlExpr[Boolean]): Update = copy(predicate = predicate.map(_ && pred).orElse(Some(pred)))
  def sql: String = 
    comment.map("/* " + _ + "*/\n").getOrElse("") + 
    "UPDATE " + table.sql + " SET " + assignments.map(_.sql).mkString(", ") + 
    predicate.map(w => " \nWHERE " + w.sql + "\n").getOrElse("")

  def params = assignments.flatMap(_.params) ++ predicate.map(_.params).getOrElse(Seq())

  def apply ()(implicit c: Connection) = try util.using(c.prepareStatement(sql)) {stmt => 
    for ((p, idx) <- params.zipWithIndex) p(stmt, idx+1)
    stmt.executeUpdate()
  } catch {
    case e: java.sql.SQLException => throw new Exception("SQL Exception ["+e.getMessage+"] when executing query ["+sql+"] with parameters: ["+params+"]")
  }

  def comment (c: String): Update = copy(comment = Some(c))
}

case class Delete (
  table: TableT,
  predicate: ast.SqlExpr[Boolean],
  comment: Option[String] = None
) {
  def where (pred: ast.SqlExpr[Boolean]) = copy(predicate = predicate && pred)
  def sql: String = comment.map("/* " + _ + "*/\n").getOrElse("") + "DELETE FROM " + table.sql + "\nWHERE " + predicate.sql + "\n"
  def apply ()(implicit c: Connection) = {
    util.using(c.prepareStatement(sql)) {stmt => 
      for ((p, idx) <- predicate.params.zipWithIndex) p(stmt, idx+1)
      stmt.executeUpdate()
    }
  }
}

case class Query (
  from:         Option[From],
  sel:          Seq[query.SelectExprS]    = Nil,
  joins:        Seq[Join]    = Nil,
  predicate:    Option[ast.SqlExpr[Boolean]]  = None,
  order:        Option[SqlS]  = None,
  group:        List[SqlS]    = Nil,
  limit:        Option[Int]           = None,
  offset:       Option[Int]           = None,
  comment:      Option[String]        = None,
  distinct:     Boolean               = false,
  forUpdateLock: Boolean              = false
) {

  // single expr, useful to have it typed for subqueries
  def select [I](expr: ast.SqlNamedExpr[I,I]): ast.SqlQueryExpr[I] = ast.SqlQueryExpr[I](select(expr: query.SelectExprS), expr)(expr.sqlTpe)

  def select (exprs: query.SelectExprS*): Query = copy(sel = exprs.toList)

  def forUpdate ()            = copy(forUpdateLock = true)
  def selectDistinct (exprs: query.SelectExprS*) = copy(sel = exprs, distinct=true)
  // TODO: remove?
  //def addCols (cols: query.ExprS*)  = copy(sel = sel ++ cols)
  def innerJoin (join: JoinBuilder) = copy(joins = joins ++ List(join.build(JoinType.Inner)))
  def leftJoin (join: JoinBuilder)  = copy(joins = joins ++ List(join.build(JoinType.Left)))

  // always append? we'll go with that for now
  def where (pred: ast.SqlExpr[Boolean]) = copy(predicate = predicate.map(_ && pred).orElse(Some(pred)))

  def orderBy (order: SqlOrdering*) = copy(
    order = Some( 
      order.toList.map(_.sql).mkString(", ") %? (order.foldLeft(List[SqlParam[_]]())((a,b) => a ++ b.params):_*)
    )
  )
  def groupBy (cols: ast.SqlExpr[_]*) = copy(
    group = cols.map(x => x: query.SqlFragmentS).toList
  )
  def limit (l: Int): Query = copy(limit = Some(l))
  def offset (o: Int): Query = copy(offset = Some(o))
  def comment (c: String): Query = copy(comment = Some(c))
  
  def as (alias: String) = new ast.SqlNamedQuery(this, alias) 

  private def optSql(prefix: String, x: Option[SqlS]) = 
    x map (prefix +~ _ +~ "\n") getOrElse ("" +~ "")

  private def listSql (prefix: String, x: Seq[SqlS], delimiter: String = "") = 
    ifStr(x.nonEmpty)(prefix) +~ x.reduceLeftOption(_ +~ delimiter +~ " \n" +~ _ ).getOrElse("" +~ "") +~ " \n"

  // Monoid append would be nice
  private def ifStr (cond: Boolean)(subj: String) = (if (cond) subj else "")

  private lazy val selectSql = listSql("SELECT " + ifStr(distinct)("DISTINCT "), sel, ",")

  private lazy val fromSql = optSql("", from.map(f => new SqlS(f.sql, f.params)))

  private lazy val joinsSql = listSql("", joins.map(j => new SqlS(j.sql, j.params)))

  private lazy val whereSql = optSql("WHERE ", predicate.map(pred => new SqlS(pred.sql, pred.params)))

  private lazy val groupBySql = listSql("GROUP BY ", group, ", ")

  private lazy val orderBySql = optSql("ORDER BY ", order)

  private lazy val limitSql = optSql("LIMIT ", limit map (_.toString))

  private lazy val offsetSql = optSql("OFFSET ", offset map (_.toString))

  lazy val statement = 
    comment.map(c => "/* " + c + "*/ \n").getOrElse("") +~
    selectSql +~ 
    fromSql +~
    joinsSql +~
    whereSql +~
    groupBySql +~
    orderBySql +~
    limitSql +~
    offsetSql +~
    (ifStr(forUpdateLock)("FOR UPDATE \n"))
  
  lazy val sql: String = {
    statement.sql
  }

  lazy val params: List[SqlParam[_]] = statement.params

  def process [B](rowParser: ResultSet => ParseResult[B])(implicit c: Connection): QueryResult[B] = new QueryResult(executeQuery(new QueryS(sql, params))(rowParser))

  def find [B](selection: Selection[B])(implicit c: Connection): QueryResult[B] = select(selection.expressions:_*) process selection
  def findDistinct [B](selection: Selection[B])(implicit c: Connection): QueryResult[B] = selectDistinct(selection.expressions:_*) process selection

  override def toString = {
    "Query(sql="+sql+", params=" + renderParams(params) +")"
  }

  def union (q: QueryS) = (sql + "\n UNION \n" + q.sql) onParams (params ++ q.params :_*)  
}

case class OrderBy (order: String, dir: String = null) {
  def sql = order + Option(dir).map(" "+_).getOrElse("")
}

