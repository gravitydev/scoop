package com.gravitydev.scoop
package query

import java.sql.{Connection, Date, Timestamp, ResultSet}
import scala.collection.mutable.ListBuffer
import util.{RsIterator, QueryResult}
import collection._, 
  parsers.{ParseResult, ParseSuccess, ParseFailure},
  ast.{SqlAssignment, SqlParamType, SqlLiteralExpr, SqlCol, SqlRawExpr, SqlWrappedExpr, SqlNamedExpr}

object `package` {
  @deprecated("Use SqlExpr[Boolean]", "0.1.23-SNAPSHOT")
  type Predicate = ast.SqlExpr[Boolean]

  // only here for backwards compat
  @deprecated("Use .find(...).list to explicitly convert QueryResult[T] to List[T]", "0.3.0-SNAPSHOT")
  implicit def queryResultToList [T](rs: util.QueryResult[T]) = rs.list
  
  implicit def stringToFragment (s: String) = new SqlFragmentS(s, Seq())
 
  // kind of hacky 
  implicit def intToLongExpr (a: SqlExpr[Int]): SqlExpr[Long] = new SqlWrappedExpr[Int,Long](a)(long)

  implicit def fragmentToQueryS (s: SqlFragmentS) = new QueryS(s.sql, s.params)

  implicit def baseToSqlLit [T](base: T)(implicit sqlType: SqlParamType[T]): SqlExpr[T] = SqlLiteralExpr(base)
  implicit def tableToWrapped [T <: Table[_]] (t: T) = new TableOps(t)
  implicit def optToSqlLit [T](base: Option[T])(implicit sqlType: SqlParamType[T]) = base map {x => SqlLiteralExpr(x)}
  implicit def baseToParam [T](base: T)(implicit sqlType: SqlParamType[T]) = SqlSingleParam(base)

  implicit def assignmentToAssignmentS (a: SqlAssignment[_]) = new AssignmentS(a.sql, a.params)
  implicit def optionalAssignmentToAssignmentS (a: Option[SqlAssignment[_]]) = a map assignmentToAssignmentS getOrElse new AssignmentS("",Nil)
  implicit def queryToQueryS (q: Query)           = new QueryS(q.sql, q.params)
  
  implicit def listToExpr (l: List[String]): List[ExprS] = l.map(x => x: ExprS)
  implicit def companionToTable [T <: Table[T]] (companion: {def apply (): T}): T = companion()
  implicit def sqlToFragment (s: SqlS) = new SqlFragmentS(s.sql, s.params)

  // should these be in the functions package?
  def exists [T:SqlParamType](query: ast.SqlQueryExpr[T]) = ast.SqlUnaryExpr[T,Boolean](query, "EXISTS", postfix=false)
  def notExists [T:SqlParamType](query: ast.SqlQueryExpr[T]) = ast.SqlUnaryExpr[T,Boolean](query, "NOT EXISTS", postfix=false) 

  // starting point
  def from (table: FromS) = Query(Some(table))
  def where (pred: PredicateS) = Query(None, predicate = Some(pred))
  def select (cols: SelectExprS*): Query = Query(None, sel = cols.toList)
  def insertInto (table: Table[_]) = new InsertBuilder(table._tableName)
  def update (table: UpdateQueryableS) = new UpdateBuilder(table)
  def deleteFrom (table: UpdateQueryableS) = new DeleteBuilder(table)

  def sql [T:SqlParamType] (sql: String): SqlRawExpr[T] = new SqlRawExpr[T](sql, Nil)
  def sql [T:SqlParamType] (sql: SqlS): SqlRawExpr[T] = new SqlRawExpr[T](sql.sql, sql.params)
  
  // necessary anymore?
  def subquery [T:SqlParamType] (q: QueryS) = sql[T]("(" +~ q +~ ")")

  // safe aliasing
  private class Aliaser {
    import scala.collection.mutable.ListBuffer
    val aliases = ListBuffer[String]()
        
    def getAlias (name: String, suffix: Int = 0): String = {
      val alias = name + (if (suffix == 0) "" else suffix.toString)
      if (aliases.contains(alias)) getAlias(name, suffix+1) else {
        aliases += alias
        alias
      }
    }
    
    def apply [A <: Table[A]] (a: TableCompanion[A]) = a as getAlias(a._alias)
  }
    
  private [this] def aliasing [T](fn: Aliaser => T) = fn(new Aliaser)

  def executeQuery [B](query: QueryS)(rowParser: ResultSet => ParseResult[B])(implicit c: Connection): Iterator[B] = try {
    val statement = c.prepareStatement(query.sql)
    val rs = statement.executeQuery()
    for ((p, idx) <- query.params.zipWithIndex) p(statement, idx+1)
    
    // iterator will close the ResultSet and the Statement
    new ResultSetIterator(rs, rowParser, {
      rs.close()
      stmt.close()
    })
  } catch {
    case e: java.sql.SQLException => throw new Exception("SQL Exception ["+e.getMessage+"] when executing query ["+query.sql+"] with parameters: ["+query.params+"]")
  }
  
  
  private type TC[A <: Table[A]] = TableCompanion[A]
  
  def using [R, A <: Table[A]](a: TC[A])(fn: A => R) = aliasing(x => fn(x(a)))
  def using [R, A <: Table[A], B <: Table[B]](a: TC[A], b: TC[B])(fn: (A,B)=>R) = aliasing(x => fn(x(a), x(b)))
  def using [R, A <: Table[A], B <: Table[B], C <: Table[C]](a: TC[A], b: TC[B], c: TC[C])(fn: (A,B,C)=>R) = aliasing(x => fn(x(a), x(b), x(c)))
  def using [R, A <: Table[A], B <: Table[B], C <: Table[C], D <: Table[D]](a: TC[A], b: TC[B], c: TC[C], d: TC[D])(fn: (A,B,C,D)=>R) = aliasing(x => fn(x(a), x(b), x(c), x(d)))
  def using [R, A <: Table[A], B <: Table[B], C <: Table[C], D <: Table[D], E <: Table[E]](a: TC[A], b: TC[B], c: TC[C], d: TC[D], e: TC[E])(fn: (A,B,C,D,E)=>R) = aliasing(x => fn(x(a), x(b), x(c), x(d), x(e)))
  def using [R, A <: Table[A], B <: Table[B], C <: Table[C], D <: Table[D], E <: Table[E], F <: Table[F]](a: TC[A], b: TC[B], c: TC[C], d: TC[D], e: TC[E], f: TC[F])(fn: (A,B,C,D,E,F)=>R) = aliasing(x => fn(x(a), x(b), x(c), x(d), x(e), x(f)))
  def using [R, A <: Table[A], B <: Table[B], C <: Table[C], D <: Table[D], E <: Table[E], F <: Table[F], G <: Table[G]](a: TC[A], b: TC[B], c: TC[C], d: TC[D], e: TC[E], f: TC[F], g: TC[G])(fn: (A,B,C,D,E,F,G)=>R) = aliasing(x => fn(x(a), x(b), x(c), x(d), x(e), x(f), x(g)))
  def using [R, A <: Table[A], B <: Table[B], C <: Table[C], D <: Table[D], E <: Table[E], F <: Table[F], G <: Table[G], H <: Table[H]](a: TC[A], b: TC[B], c: TC[C], d: TC[D], e: TC[E], f: TC[F], g: TC[G], h: TC[H])(fn: (A,B,C,D,E,F,G,H)=>R) = aliasing(x => fn(x(a), x(b), x(c), x(d), x(e), x(f), x(g), x(h)))
  def using [R, A <: Table[A], B <: Table[B], C <: Table[C], D <: Table[D], E <: Table[E], F <: Table[F], G <: Table[G], H <: Table[H], I <: Table[I]](a: TC[A], b: TC[B], c: TC[C], d: TC[D], e: TC[E], f: TC[F], g: TC[G], h: TC[H], i: TC[I])(fn: (A,B,C,D,E,F,G,H,I)=>R) = aliasing(x => fn(x(a), x(b), x(c), x(d), x(e), x(f), x(g), x(h), x(i)))
  def using [R, A <: Table[A], B <: Table[B], C <: Table[C], D <: Table[D], E <: Table[E], F <: Table[F], G <: Table[G], H <: Table[H], I <: Table[I], J <: Table[J]](a: TC[A], b: TC[B], c: TC[C], d: TC[D], e: TC[E], f: TC[F], g: TC[G], h: TC[H], i: TC[I], j: TC[J])(fn: (A,B,C,D,E,F,G,H,I,J)=>R) = aliasing(x => fn(x(a), x(b), x(c), x(d), x(e), x(f), x(g), x(h), x(i), x(j)))
  def using [R, A <: Table[A], B <: Table[B], C <: Table[C], D <: Table[D], E <: Table[E], F <: Table[F], G <: Table[G], H <: Table[H], I <: Table[I], J <: Table[J], K <: Table[K]](a: TC[A], b: TC[B], c: TC[C], d: TC[D], e: TC[E], f: TC[F], g: TC[G], h: TC[H], i: TC[I], j: TC[J], k: TC[K])(fn: (A,B,C,D,E,F,G,H,I,J,K)=>R) = aliasing(x => fn(x(a), x(b), x(c), x(d), x(e), x(f), x(g), x(h), x(i), x(j), x(k)))
  def using [R, A <: Table[A], B <: Table[B], C <: Table[C], D <: Table[D], E <: Table[E], F <: Table[F], G <: Table[G], H <: Table[H], I <: Table[I], J <: Table[J], K <: Table[K], L <: Table[L]](a: TC[A], b: TC[B], c: TC[C], d: TC[D], e: TC[E], f: TC[F], g: TC[G], h: TC[H], i: TC[I], j: TC[J], k: TC[K], l: TC[L])(fn: (A,B,C,D,E,F,G,H,I,J,K,L)=>R) = aliasing(x => fn(x(a), x(b), x(c), x(d), x(e), x(f), x(g), x(h), x(i), x(j), x(k), x(l)))
  def using [R, A <: Table[A], B <: Table[B], C <: Table[C], D <: Table[D], E <: Table[E], F <: Table[F], G <: Table[G], H <: Table[H], I <: Table[I], J <: Table[J], K <: Table[K], L <: Table[L], M <: Table[M]](a: TC[A], b: TC[B], c: TC[C], d: TC[D], e: TC[E], f: TC[F], g: TC[G], h: TC[H], i: TC[I], j: TC[J], k: TC[K], l: TC[L], m: TC[M])(fn: (A,B,C,D,E,F,G,H,I,J,K,L,M)=>R) = aliasing(x => fn(x(a), x(b), x(c), x(d), x(e), x(f), x(g), x(h), x(i), x(j), x(k), x(l), x(m)))
  def using [R, A <: Table[A], B <: Table[B], C <: Table[C], D <: Table[D], E <: Table[E], F <: Table[F], G <: Table[G], H <: Table[H], I <: Table[I], J <: Table[J], K <: Table[K], L <: Table[L], M <: Table[M], N <: Table[N]](a: TC[A], b: TC[B], c: TC[C], d: TC[D], e: TC[E], f: TC[F], g: TC[G], h: TC[H], i: TC[I], j: TC[J], k: TC[K], l: TC[L], m: TC[M], n: TC[N])(fn: (A,B,C,D,E,F,G,H,I,J,K,L,M,N)=>R) = aliasing(x => fn(x(a), x(b), x(c), x(d), x(e), x(f), x(g), x(h), x(i), x(j), x(k), x(l), x(m), x(n)))
  def using [R, A <: Table[A], B <: Table[B], C <: Table[C], D <: Table[D], E <: Table[E], F <: Table[F], G <: Table[G], H <: Table[H], I <: Table[I], J <: Table[J], K <: Table[K], L <: Table[L], M <: Table[M], N <: Table[N], O <: Table[O]](a: TC[A], b: TC[B], c: TC[C], d: TC[D], e: TC[E], f: TC[F], g: TC[G], h: TC[H], i: TC[I], j: TC[J], k: TC[K], l: TC[L], m: TC[M], n: TC[N], o: TC[O])(fn: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O)=>R) = aliasing(x => fn(x(a), x(b), x(c), x(d), x(e), x(f), x(g), x(h), x(i), x(j), x(k), x(l), x(m), x(n), x(o)))
  def using [R, A <: Table[A], B <: Table[B], C <: Table[C], D <: Table[D], E <: Table[E], F <: Table[F], G <: Table[G], H <: Table[H], I <: Table[I], J <: Table[J], K <: Table[K], L <: Table[L], M <: Table[M], N <: Table[N], O <: Table[O], P <: Table[P]](a: TC[A], b: TC[B], c: TC[C], d: TC[D], e: TC[E], f: TC[F], g: TC[G], h: TC[H], i: TC[I], j: TC[J], k: TC[K], l: TC[L], m: TC[M], n: TC[N], o: TC[O], p: TC[P])(fn: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P)=>R) = aliasing(x => fn(x(a), x(b), x(c), x(d), x(e), x(f), x(g), x(h), x(i), x(j), x(k), x(l), x(m), x(n), x(o), x(p)))
  def using [R, A <: Table[A], B <: Table[B], C <: Table[C], D <: Table[D], E <: Table[E], F <: Table[F], G <: Table[G], H <: Table[H], I <: Table[I], J <: Table[J], K <: Table[K], L <: Table[L], M <: Table[M], N <: Table[N], O <: Table[O], P <: Table[P], Q <: Table[Q]](a: TC[A], b: TC[B], c: TC[C], d: TC[D], e: TC[E], f: TC[F], g: TC[G], h: TC[H], i: TC[I], j: TC[J], k: TC[K], l: TC[L], m: TC[M], n: TC[N], o: TC[O], p: TC[P], q: TC[Q])(fn: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q)=>R) = aliasing(x => fn(x(a), x(b), x(c), x(d), x(e), x(f), x(g), x(h), x(i), x(j), x(k), x(l), x(m), x(n), x(o), x(p), x(q)))
  def using [R, A <: Table[A], B <: Table[B], C <: Table[C], D <: Table[D], E <: Table[E], F <: Table[F], G <: Table[G], H <: Table[H], I <: Table[I], J <: Table[J], K <: Table[K], L <: Table[L], M <: Table[M], N <: Table[N], O <: Table[O], P <: Table[P], Q <: Table[Q], S <: Table[S]](a: TC[A], b: TC[B], c: TC[C], d: TC[D], e: TC[E], f: TC[F], g: TC[G], h: TC[H], i: TC[I], j: TC[J], k: TC[K], l: TC[L], m: TC[M], n: TC[N], o: TC[O], p: TC[P], q: TC[Q], s: TC[S])(fn: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,S)=>R) = aliasing(x => fn(x(a), x(b), x(c), x(d), x(e), x(f), x(g), x(h), x(i), x(j), x(k), x(l), x(m), x(n), x(o), x(p), x(q), x(s)))
}

class TableOps [T <: Table[_]](t: T) {
  def on (pred: SqlExpr[Boolean]) = Join(t.sql, pred.sql, pred.params)
}

case class Join (table: String, predicate: String, params: Seq[SqlParam[_]]) {
  def sql: String = table + " ON " + predicate
}

class InsertBuilder (into: String) {
  def set (assignments: AssignmentS*) = Insert (into, assignments.map(_.sql).filter(_.nonEmpty).toList, assignments.foldLeft(Seq[SqlParam[_]]()){(a,b) => a ++ b.params})
  def values (assignments: SqlAssignment[_]*) = Insert2(into, assignments.filter(_.sql.nonEmpty).toList)
  def apply (columns: SqlCol[_]*) = new InsertBuilder2(into, columns)
}
class InsertBuilder2 (into: String, columns: Seq[SqlCol[_]]) {
  def values (sql: SqlS) = Insert3(into, columns, sql)
}

class UpdateBuilder (tb: UpdateQueryableS) {
  def set (assignments: AssignmentS*) = Update (tb.sql, assignments.map(_.sql).filter(_.nonEmpty).toList, None, assignments.foldLeft(Seq[SqlParam[_]]()){(a,b) => a ++ b.params})
}

class DeleteBuilder (tb: UpdateQueryableS) {
  def where (pred: PredicateS) = new Delete(tb.sql, pred.sql, params = pred.params)
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
  def onDuplicateKeyUpdate (assignments: AssignmentS*) = Upsert(this, assignments.map(_.sql).filter(_.nonEmpty).toList, assignments.foldLeft(Seq[SqlParam[_]]()){(a,b) => a ++ b.params})
}

case class Insert (
  into: String,
  assignments: List[String] = Nil,
  params: Seq[SqlParam[_]] = Nil,
  comment: Option[String] = None
) extends InsertBase {
  def comment (c: String): Insert = copy(comment = Some(c))
  def sql: String = "INSERT INTO " + into + " SET " + assignments.mkString(", ")
}

// alternate syntax
case class Insert2 (
  into: String,
  assignments: List[SqlAssignment[_]],
  comment: Option[String] = None
) extends InsertBase {
  def comment (c: String): Insert2 = copy(comment = Some(c))
  def params: Seq[SqlParam[_]] = assignments.foldLeft(Seq[SqlParam[_]]()){(a,b) => a ++ b.params}
  def sql: String = "INSERT INTO " + into + " (" + assignments.map(_.col.columnName).mkString(", ") + ") VALUES (" + assignments.map(_.valueSql).mkString(", ") + ")"
}

case class Upsert (
  insert: InsertBase,
  assignments: List[String] = Nil,
  updateParams: Seq[SqlParam[_]]  = Nil
) extends InsertBase {
  def sql = insert.sql + " ON DUPLICATE KEY UPDATE " + assignments.mkString(", ")
  def params = insert.params ++ updateParams
}

// using a subselect
case class Insert3 (
  into: String,
  columns: Seq[SqlCol[_]],
  query: SqlS,
  comment: Option[String] = None
) extends InsertBase {
  def comment (c: String): Insert3 = copy(comment = Some(c))
  def params: Seq[SqlParam[_]] = query.params 
  def sql: String = "INSERT INTO " + into + " (" + columns.map(_.columnName).mkString(", ") + ")\n" + query.sql
}

case class Update (
  table: String,
  assignments: List[String] = Nil,
  predicate: Option[String] = None,
  params: Seq[SqlParam[_]]  = Nil,
  comment: Option[String] = None
) {
  def where (pred: PredicateS) = copy(predicate = predicate.map(_ + " AND " + pred.sql).orElse(Some(pred.sql)), params = this.params ++ pred.params)
  def sql: String = comment.map("/* " + _ + "*/\n").getOrElse("") + "UPDATE " + table + " SET " + assignments.mkString(", ") + predicate.map(w => " \nWHERE " + w + "\n").getOrElse("")

  def apply ()(implicit c: Connection) = try util.using(c.prepareStatement(sql)) {stmt => 
    for ((p, idx) <- params.zipWithIndex) p(stmt, idx+1)
    stmt.executeUpdate()
  } catch {
    case e: java.sql.SQLException => throw new Exception("SQL Exception ["+e.getMessage+"] when executing query ["+sql+"] with parameters: ["+params+"]")
  }

  def comment (c: String): Update = copy(comment = Some(c))
}

case class Delete (
  table: String,
  predicate: String,
  params: Seq[SqlParam[_]] = Nil,
  comment: Option[String] = None
) {
  def where (pred: PredicateS) = copy(predicate = predicate + " AND " + pred.sql, params = this.params ++ pred.params)
  def sql: String = comment.map("/* " + _ + "*/\n").getOrElse("") + "DELETE FROM " + table + "\nWHERE " + predicate + "\n"
  def apply ()(implicit c: Connection) = {
    util.using(c.prepareStatement(sql)) {stmt => 
      for ((p, idx) <- params.zipWithIndex) p(stmt, idx+1)
      stmt.executeUpdate()
    }
  }
}

case class Query (
  from:         Option[SqlS],
  sel:          List[SqlS]    = List("*"),
  joins:        List[SqlS]    = Nil,
  predicate:    Option[SqlS]  = None,
  order:        Option[SqlS]  = None,
  group:        List[SqlS]    = Nil,
  limit:        Option[Int]           = None,
  offset:       Option[Int]           = None,
  comment:      Option[String]        = None,
  distinct:     Boolean               = false,
  forUpdateLock: Boolean              = false
) {

  // single expr, useful to have it typed for subqueries
  def select [T:SqlParamType](expr: SqlNamedExpr[T]): ast.SqlQueryExpr[T] = ast.SqlQueryExpr[T](select(expr: SelectExprS))

  def select (cols: SelectExprS*): Query = copy(sel = cols.toList)
  def forUpdate ()            = copy(forUpdateLock = true)
  def selectDistinct (cols: SelectExprS*) = copy(sel = cols.map(_.sql).toList, distinct=true)
  def addCols (cols: ExprS*)  = copy(sel = sel ++ cols.toList)
  def innerJoin (join: JoinS) = copy(joins = joins ++ List("INNER JOIN " +~ join))
  def leftJoin (join: JoinS)  = copy(joins = joins ++ List("LEFT JOIN " +~ join))

  // always append? we'll go with that for now
  def where (pred: PredicateS) = copy(predicate = predicate.map(_ +~ " AND " +~ pred).orElse(Some(pred)))

  def orderBy (order: OrderByS*) = copy(
    order = Some( 
      order.toList.map(_.sql).mkString(", ") %? (order.foldLeft(Seq[SqlParam[_]]())((a,b) => a ++ b.params):_*)
    )
  )
  def groupBy (cols: ExprS*) = copy(
    group = cols.map(x => x:SqlFragmentS).toList
  )
  def limit (l: Int): Query = copy(limit = Some(l))
  def offset (o: Int): Query = copy(offset = Some(o))
  def comment (c: String): Query = copy(comment = Some(c))
  
  def as (alias: String) = new AliasedSqlFragmentS(sql, alias, params)

  private def optSql(prefix: String, x: Option[SqlS]) = 
    x map (prefix +~ _ +~ "\n") getOrElse ("" +~ "")

  private def listSql (prefix: String, x: Seq[SqlS], delimiter: String = "") = 
    ifStr(x.nonEmpty)(prefix) +~ x.reduceLeftOption(_ +~ delimiter +~ " \n" +~ _ +~ " \n").getOrElse("" +~ "") +~ " \n"

  // Monoid append would be nice
  private def ifStr (cond: Boolean)(subj: String) = (if (cond) subj else "")

  private lazy val selectSql = listSql("SELECT " + ifStr(distinct)("DISTINCT "), sel, ",")

  private lazy val fromSql = optSql("FROM ", from)

  private lazy val joinsSql = listSql("", joins)

  private lazy val whereSql = optSql("WHERE ", predicate)

  private lazy val groupBySql = listSql("GROUP BY ", group, ",")

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
  
  lazy val sql = statement.sql

  lazy val params = statement.params

  @deprecated("Use process", "0.2.5-SNAPSHOT") 
  def map [B](process: ResultSet => ParseResult[B])(implicit c: Connection): List[B] = executeQuery(new QueryS(sql, params))(process).toList

  def process [B](rowParser: ResultSet => ParseResult[B])(implicit c: Connection): Iterator[B] = executeQuery(new QueryS(sql, params))(rowParser)

  def find [B](parser: ResultSetParser[B])(implicit c: Connection): QueryResult[B] = new QueryResult(select(parser.columns:_*) process parser)
  def findDistinct [B](parser: ResultSetParser[B])(implicit c: Connection): QueryResult[B] = new QueryResult(selectDistinct(parser.columns:_*) process parser)

  override def toString = {
    "Query(sql="+sql+", params=" + renderParams(params) +")"
  }

  def union (q: QueryS) = (sql + "\n UNION \n" + q.sql) onParams (params ++ q.params :_*)
  
  // useful for subqueries
  //def as (alias: String) = "(" +~ queryToQueryS(this) +~ ") as " +~ alias
}

case class OrderBy (order: String, dir: String = null) {
  def sql = order + Option(dir).map(" "+_).getOrElse("")
}

