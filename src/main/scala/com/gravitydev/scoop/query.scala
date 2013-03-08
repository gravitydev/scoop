package com.gravitydev.scoop
package query

import java.sql.{Connection, Date, Timestamp, ResultSet}
import scala.collection.mutable.ListBuffer
import collection._, ast._

object `package` {
  type Predicate = ast.SqlExpr[Boolean]
  
  implicit def stringToFragment (s: String) = new SqlFragmentS(s, Seq())
  implicit def fromExpr (expr: ast.SqlExpr[_]) = new ExprS(expr.sql, expr.params)
  
  implicit def tableToWrapped [T <: SqlTable[_]] (t: T) = new TableWrapper(t)
  implicit def baseToSqlLit [T](base: T)(implicit sqlType: SqlType[T]) = SqlLiteralExpr(base)
  implicit def intToSqlLongLit (base: Int): SqlLiteralExpr[Long] = SqlLiteralExpr(base: Long)
  implicit def optToSqlLit [T](base: Option[T])(implicit sqlType: SqlType[T]) = base map {x => SqlLiteralExpr(x)}
  implicit def baseToParam [T](base: T)(implicit sqlType: SqlType[T]) = SqlSingleParam(base)

  implicit def toJoin (s: String)         = new JoinS(s, Nil)
  implicit def toPredicate (s: String)    = new PredicateS(s, Nil)
  //implicit def toQuery (s: String)        = new QueryS(s, Seq())
  implicit def predicateToQueryS (p: PredicateS) = new QueryS(p.sql, p.params)
  implicit def querySToPredicate (p: QueryS) = new PredicateS(p.sql, p.params)
  implicit def toOrder (s: String)        = new OrderByS(s)
  
  implicit def tableToUpdate(t: SqlTable[_])      = new UpdateQueryableS(t.sql)
  implicit def joinToJoin (j: Join)               = new JoinS(j.sql, j.params)
  implicit def predToPredicateS (pred: SqlExpr[Boolean]) = new PredicateS(pred.sql, pred.params)
  implicit def orderingToOrder (o: SqlOrdering)   = new OrderByS(o.sql)
  implicit def assignmentToAssignmentS (a: SqlAssignment[_]) = new AssignmentS(a.sql, a.params)
  implicit def queryToQueryS (q: Query)           = new QueryS(q.sql, q.params)
  
  implicit def listToExpr (l: List[String]) = l.map(x => x: ExprS)
  implicit def companionToTable [T <: ast.SqlTable[T]] (companion: {def apply (): T}): T = companion()

  // query util

  def exists [T:SqlType](query: ast.SqlQueryExpr[T]) = ast.SqlUnaryExpr[T,Boolean](query, "EXISTS", postfix=false)
  def notExists [T:SqlType](query: ast.SqlQueryExpr[T]) = ast.SqlUnaryExpr[T,Boolean](query, "NOT EXISTS", postfix=false)
  

  // starting point
  def from (table: FromS) = Query(table.sql)
  def insertInto (table: SqlTable[_]) = new InsertBuilder(table._tableName)
  def update (table: UpdateQueryableS) = new UpdateBuilder(table)
  def deleteFrom (table: UpdateQueryableS) = new DeleteBuilder(table)

  def sql [T:SqlType] (sql: String): SqlRawExpr[T] = new SqlRawExpr[T](sql, Nil)
  def sql [T:SqlType] (sql: SqlS): SqlRawExpr[T] = new SqlRawExpr[T](sql.sql, sql.params)
  def subquery [T:SqlType] (q: QueryS) = sql[T]("(" +~ q +~ ")")

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

  def executeQuery [B](query: QueryS)(process: ResultSet => ParseResult[B])(implicit c: Connection): List[B] = try {
    util.using (c.prepareStatement(query.sql)) {statement =>
      for ((p, idx) <- query.params.zipWithIndex) p(statement, idx+1)
      
      util.using (statement.executeQuery()) {results => 
        util.bmap(results.next) { 
          process(results) match {
            case ParseSuccess(v) => v
            case ParseFailure(e) => sys.error("Scoop Parse Error: " + e)
          }
        }
      }
    }
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

class TableWrapper [T <: SqlTable[_]](t: T) {
  def on (pred: SqlExpr[Boolean]) = Join(t.sql, pred.sql, pred.params)
}

case class Join (table: String, predicate: String, params: Seq[SqlParam[_]]) {
  def sql: String = table + " ON " + predicate
}

class InsertBuilder (into: String) {
  def set (assignments: AssignmentS*) = Insert (into, assignments.map(_.sql).toList, assignments.foldLeft(Seq[SqlParam[_]]()){(a,b) => a ++ b.params})
  def values (assignments: SqlAssignment[_]*) = Insert2(into, assignments.toList)
  def apply (columns: SqlCol[_]*) = new InsertBuilder2(into, columns)
}
class InsertBuilder2 (into: String, columns: Seq[SqlCol[_]]) {
  def values (sql: SqlS) = Insert3(into, columns, sql)
}

class UpdateBuilder (tb: UpdateQueryableS) {
  def set (assignments: AssignmentS*) = Update (tb.sql, assignments.map(_.sql).toList, None, assignments.foldLeft(Seq[SqlParam[_]]()){(a,b) => a ++ b.params})
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
        case _ => None
      }
    }
  } catch {
    case e: java.sql.SQLException => throw new Exception("SQL Exception ["+e.getMessage+"] when executing query ["+sql+"] with parameters: ["+params+"]")
  }
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
  from:       String,
  sel:        List[String]    = List("*"),
  joins:      List[String]    = Nil,
  predicate:  Option[String]  = None,
  order:      Option[String]  = None,
  group:      List[String]    = Nil,
  selectParams: Seq[SqlParam[_]] = Nil,
  queryParams: Seq[SqlParam[_]] = Nil,
  limit:      Option[Int]     = None,
  offset:     Option[Int]     = None,
  comment:    Option[String]  = None,
  distinct:   Boolean         = false,
  forUpdateLock:  Boolean         = false
) {

  // single expr, useful to have it typed
  def select [T:SqlType](expr: SqlNamedExpr[T]): ast.SqlQueryExpr[T] = ast.SqlQueryExpr[T](select(expr: SelectExprS))

  def select (cols: SelectExprS*): Query = copy(sel = cols.map(_.sql).toList, selectParams = cols.map(_.params).flatten)
  def forUpdate ()            = copy(forUpdateLock = true)
  def selectDistinct (cols: SelectExprS*) = copy(sel = cols.map(_.sql).toList, distinct=true)
  def addCols (cols: ExprS*)  = copy(sel = sel ++ cols.map(_.sql).toList)
  def innerJoin (join: JoinS) = copy(joins = joins ++ List("INNER JOIN " + join.sql), queryParams = this.queryParams ++ join.params )
  def leftJoin (join: JoinS)  = copy(joins = joins ++ List("LEFT JOIN " + join.sql), queryParams = this.queryParams ++ join.params)

  // always append? we'll go with that for now
  def where (pred: PredicateS) = copy(predicate = predicate.map(_ + " AND " + pred.sql).orElse(Some(pred.sql)), queryParams = this.queryParams ++ pred.params)

  def orderBy (order: OrderByS*) = copy(order = Some( (order.toList.map(_.sql)).mkString(", ")) )
  def groupBy (cols: ExprS*) = copy(group = cols.map(_.sql).toList)
  def limit (l: Int): Query = copy(limit = Some(l))
  def offset (o: Int): Query = copy(offset = Some(o))
  def comment (c: String): Query = copy(comment = Some(c))
  
  def params: Seq[SqlParam[_]] = selectParams ++ queryParams
  
  def sql: String = 
    comment.map(c => "/* " + c + "*/ \n").getOrElse("") +
    "SELECT " + (if (distinct) "DISTINCT " else "") + sel.mkString(", \n") + " \n" + 
    "FROM " + from + "\n" +
    joins.mkString("", "\n", "\n") + 
    predicate.map(w => "WHERE " + w + "\n").getOrElse("") + 
    (if (group.nonEmpty) group.mkString("GROUP BY ", ", ", "\n") else "") +
    order.map("ORDER BY " + _ + "\n").getOrElse("") +
    limit.map("LIMIT " + _ + "\n").getOrElse("") +
    offset.map("OFFSET " + _ + "\n").getOrElse("") + 
    (if (forUpdateLock) "FOR UPDATE \n" else "")
 
  def map [B](process: ResultSet => ParseResult[B])(implicit c: Connection): List[B] = executeQuery(new QueryS(sql, params))(process)

  def find [B](parser: ResultSetParser[B])(implicit c: Connection): List[B] = select(parser.columns:_*) map parser 
  def findDistinct [B](parser: ResultSetParser[B])(implicit c: Connection): List[B] = selectDistinct(parser.columns:_*) map parser 

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

