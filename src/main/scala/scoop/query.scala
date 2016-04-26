package com.gravitydev.scoop
package query

import java.sql.{Connection, Date, Timestamp, ResultSet}
import scala.collection.mutable.ListBuffer
import util.{ResultSetIterator, QueryResult}
import scala.collection._, 
  ast.{Query, Join, SqlType, SqlParseStrictExpr, SqlLiteralExpr, /*SqlCol, */SqlRawExpr, SqlNamedExpr}
import builder.{DeleteBuilder, QueryBuilder, InsertBuilder, UpdateBuilder}

case class TableBuilder [A](table: A)
object TableBuilder {
  implicit def fromTable[T] (table: T) = TableBuilder(table)
  implicit def fromCompanion[T](tableComp: () => T) = TableBuilder(tableComp())
}


object `package` extends builder.QueryBuilderBase {
  @deprecated("Use SqlExpr[Boolean]", "0.1.23-SNAPSHOT")
  type Predicate = ast.SqlExpr[Boolean]

  /** Shadow this to use a different dialog */
  implicit def sqlDialect: SqlDialect = BaseSqlDialect

  implicit def stringToFragment (s: String) = ParameterizedSql(s, Nil)
 
  // kind of hacky 
  implicit def intToLongExpr (a: SqlExpr[Int]): SqlExpr[Long] = a.cast[Long] 

  implicit def baseToSqlLit [T](base: T)(implicit sqlType: SqlType[T]): SqlParseStrictExpr[T] = SqlLiteralExpr(base)
  //implicit def tableToWrapped [T <: definition.TableT] (t: T) = new TableOps(t)
  implicit def optToSqlLit [T](base: Option[T])(implicit sqlType: SqlType[T]) = base map {x => SqlLiteralExpr(x)}
  implicit def baseToParam [T](base: T)(implicit sqlType: SqlType[T]) = SqlSingleParam(base)

  implicit def queryToSql (q: builder.Query[_])(implicit dialect: SqlDialect) = dialect.toParameterizedSql(q)
  
  implicit def companionToTable [T <: Table[T]] (companion: {def apply (): T}): T = companion()

  implicit def fromColumn [T:SqlType](t: definition.Column[T]): ast.Column[T] = ast.Column[T](t.table._alias getOrElse t.table._tableName, t.name)

  implicit def nonNullableColumnToSelection [T](col: definition.NonNullableColumn[T]): parsers.Selection1[T] = new parsers.Selection1[T](
    rs => col.sqlTpe.parseOr(rs, col.name, s"Cannot parse [$col] from [$rs]"),
    List(col)
  )
  implicit def nullableColumnToSelection [T](col: definition.NullableColumn[T]): parsers.Selection1[Option[T]] = new parsers.Selection1[Option[T]](
    rs => Right(col.sqlTpe.parse(rs, col.name)),
    List(col)
  )

  implicit class TableOps [T <: definition.TableT](t: T) {
    def on (pred: SqlExpr[Boolean]) = 
      new builder.JoinBuilder(
        ast.Table(t._tableName, t._schema), 
        pred
      )
  }

  implicit class NonNullableColumnOps[T:SqlType](col: definition.NonNullableColumn[T]) {
    def as (alias: String) = ast.SqlNamedStrictExpr(col, alias)
    def := (x: SqlExpr[T]) = ast.SqlAssignment(col.name, x)
  }
  implicit class NullableColumnOps[T:SqlType](col: definition.NullableColumn[T]) {
    def as (alias: String) = ast.SqlNamedOptExpr(col, alias)
    def := (x: Option[T]): ast.SqlAssignment = ast.SqlAssignment(col.name, x map (v => ast.SqlLiteralExpr(v)) getOrElse ast.SqlNull)
    def := (x: SqlExpr[Option[T]]): ast.SqlAssignment = ast.SqlAssignment(col.name, x)
  }

  def exists [T:SqlType](query: builder.Query[T]) = ast.SqlUnaryExpr[T,Boolean](new ast.SqlQueryExpr(query), "EXISTS", postfix=false)
  def notExists [T:SqlType](query: ast.SqlQueryExpr[T]) = ast.SqlUnaryExpr[T,Boolean](query, "NOT EXISTS", postfix=false) 

  // starting point
  def from (table: ast.Queryable) = QueryBuilder(Some(table))
  def where (pred: ast.SqlExpr[Boolean]) = QueryBuilder(None, predicate = Some(pred))
  def insertInto (table: ast.Table) = InsertBuilder.Into(table)
  def update (table: ast.Table) = UpdateBuilder.Target(table)
  def deleteFrom (table: ast.Table) = DeleteBuilder.From(table)

  def sqlExpr [T:SqlType] (sql: ParameterizedSql): SqlParseStrictExpr[T] = SqlRawExpr[T](sql)
 
  protected def buildQuery [X](sel: Selection[X]) = Query(sel)
  
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
    
    def apply [A <: Table[A]] (a: TableBuilder[A]): A = a.table as getAlias(a.table._alias getOrElse a.table._tableName)
  }
    
  private [this] def aliasing [T](fn: Aliaser => T) = fn(new Aliaser)

  def executeQuery [B](query: ParameterizedSql)(rowParser: ResultSet => ParseResult[B])(implicit c: Connection): Iterator[B] = try {
    val statement = c.prepareStatement(query.sql)
    for ((p, idx) <- query.params.zipWithIndex) p(statement, idx+1)
    val rs = statement.executeQuery()
    
    // iterator will close the ResultSet and the Statement
    new ResultSetIterator(rs, rowParser, {
      rs.close()
      statement.close()
    })
  } catch {
    case e: java.sql.SQLException => throw new Exception("SQL Exception ["+e.getMessage+"] when executing query ["+query.sql+"] with parameters: ["+query.params+"]")
  }
  
  
  //private type TC[A <: Table[A]] = TableCompanion[A]
  //private type TC[A <: Table[A]] = () => A //TableCompanion[A]
  private type TC[A <: Table[A]] = TableBuilder[A]
  
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


