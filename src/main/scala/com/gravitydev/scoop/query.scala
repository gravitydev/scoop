package com.gravitydev.scoop
package query

import java.sql.{Connection, Date, Timestamp, ResultSet}
import scala.collection.mutable.ListBuffer
import collection._, ast._

object `package` {
  
  implicit def tableToWrapped [T <: SqlTable[_]] (t: T) = new TableWrapper(t)
  implicit def baseToSqlLit [T](base: T)(implicit sqlType: SqlType[T]) = SqlLiteralExpr(base)
  implicit def baseToParam [T](base: T)(implicit sqlType: SqlType[T]) = SqlSingleParam(base)

  implicit def toFrom (s: String)         = new FromS(s)
  implicit def toExpr (s: String)         = new ExprS(s)
  implicit def toSelectExpr (s: String)   = new SelectExprS(s)
  implicit def toJoin (s: String)         = new JoinS(s, Nil)
  implicit def toPredicate (s: String)    = new PredicateS(s, Nil)
  implicit def toOrder (s: String)        = new OrderByS(s)
  
  implicit def colToExprS (col: SqlCol[_])        = new ExprS(col.sql)
  implicit def colToSelectExprS (col: SqlCol[_])  = new SelectExprS(col.selectSql)
  implicit def tableToFrom (t: SqlTable[_])       = new FromS(t.sql)
  implicit def joinToJoin (j: Join)               = new JoinS(j.sql, j.params)
  implicit def predToPredicateS (pred: SqlExpr[Boolean]) = new PredicateS(pred.sql, pred.params)
  implicit def orderingToOrder (o: SqlOrdering)   = new OrderByS(o.sql)
  implicit def assignmentToAssignmentS (a: SqlAssignment[_]) = new AssignmentS(a.sql, a.params)
  
  implicit def listToExpr (l: List[String]) = l.map(x => x: ExprS)
  implicit def companionToTable [T <: ast.SqlTable[T]] (companion: {def apply (): T}): T = companion()

  // starting point
  def from (table: FromS) = Query(table.sql)
  def insertInto (table: SqlTable[_]) = new InsertBuilder(table._tableName)
  def update (table: FromS) = new UpdateBuilder(table)

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
}

class TableWrapper [T <: SqlTable[_]](t: T) {
  def on (pred: SqlExpr[Boolean]) = Join(t.sql, pred.sql, pred.params)
}

case class Join (table: String, predicate: String, params: Seq[SqlParam[_]]) {
  def sql = table + " ON " + predicate
}

sealed abstract class SqlS (val sql: String) {
  override def toString = getClass.getName + "(" + sql + ")"
}
class ExprS       (s: String) extends SqlS(s)
class SelectExprS (s: String) extends SqlS(s)
class FromS       (s: String) extends SqlS(s)
class JoinS       (s: String, val params: Seq[SqlParam[_]]) extends SqlS(s)

class PredicateS (s: String, val params: Seq[SqlParam[_]]) extends SqlS(s) {
  def onParams (p: SqlParam[_]*): PredicateS = new PredicateS(s, params ++ p.toSeq)
}

class OrderByS   (s: String) extends SqlS(s)
class AssignmentS (s: String, val params: Seq[SqlParam[_]]) extends SqlS(s)

class InsertBuilder (into: String) {
  def set (assignments: AssignmentS*) = Insert (into, assignments.map(_.sql).toList, assignments.foldLeft(Seq[SqlParam[_]]()){(a,b) => a ++ b.params})
}

class UpdateBuilder (tb: FromS) {
  def set (assignments: AssignmentS*) = Update (tb.sql, assignments.map(_.sql).toList, None, assignments.foldLeft(Seq[SqlParam[_]]()){(a,b) => a ++ b.params})
}

case class Insert (
  into: String,
  assignments: List[String] = Nil,
  params: Seq[SqlParam[_]] = Nil
) {
  def sql = "INSERT INTO " + into + " SET " + assignments.mkString(", ")
  def apply ()(implicit c: Connection) = {
    import java.sql.Statement
    util.using(c.prepareStatement(sql, Statement.RETURN_GENERATED_KEYS)) {stmt => 
      stmt.executeUpdate()

      util.using(stmt.getGeneratedKeys()) {rs =>
        rs.next()
        rs.getLong(1)
      }
    }
  }
}

case class Update (
  table: String,
  assignments: List[String] = Nil,
  predicate: Option[String] = None,
  params: Seq[SqlParam[_]]  = Nil
) {
  def where (pred: PredicateS) = copy(predicate = predicate.map(_ + " AND " + pred.sql).orElse(Some(pred.sql)), params = this.params ++ pred.params)
  def sql = "UPDATE " + table + " SET " + assignments.mkString(", ") + predicate.map(w => " \nWHERE " + w + "\n").getOrElse("")
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
  params:     Seq[SqlParam[_]] = Nil,
  limit:      Option[Int]     = None,
  offset:     Option[Int]     = None
) {
  def select (cols: SelectExprS*)   = copy(sel = cols.map(_.sql).toList)
  def addCols (cols: ExprS*)  = copy(sel = sel ++ cols.map(_.sql).toList)
  def innerJoin (join: JoinS) = copy(joins = joins ++ List("INNER JOIN " + join.sql), params = this.params ++ join.params )
  def leftJoin (join: JoinS)  = copy(joins = joins ++ List("LEFT JOIN " + join.sql), params = this.params ++ join.params)

  // not sure if these should exist (where methods that overwrite the previous where clause)  
  //def where (predicate: PredicateS) = copy(predicate = Some(predicate.sql), params = this.params ++ predicate.params)

  // i don't think this is necessary anymore
  //def where (predicate: SqlExpr[Boolean]) = copy(predicate = Some(predicate.sql), params = this.params ++ predicate.params)

  // always append? we'll go with that for now
  def where (pred: PredicateS) = copy(predicate = predicate.map(_ + " AND " + pred.sql).orElse(Some(pred.sql)), params = this.params ++ pred.params)

  def orderBy (order: OrderByS*) = copy(order = Some( (order.toList.map(_.sql)).mkString(", ")) )
  def groupBy (cols: ExprS*) = copy(group = cols.map(_.sql).toList)
  def limit (l: Int): Query = copy(limit = Some(l))
  def offset (o: Int): Query = copy(offset = Some(o))
  
  def sql = 
    "SELECT " + sel.mkString(", \n") + " \n" + 
    "FROM " + from + "\n" +
    joins.mkString("", "\n", "\n") + 
    predicate.map(w => "WHERE " + w + "\n").getOrElse("") + 
    (if (group.nonEmpty) group.mkString("GROUP BY ", ", ", "\n") else "") +
    order.map("ORDER BY " + _ + "\n").getOrElse("") +
    limit.map("LIMIT " + _ + "\n").getOrElse("") +
    offset.map("OFFSET " + _ + "\n").getOrElse("")
 
  def map [B](process: ResultSet => ParseResult[B])(implicit c: Connection): List[B] = try {
    util.using (c.prepareStatement(sql)) {statement =>
      for ((p, idx) <- params.zipWithIndex) p(statement, idx+1)
      
      util.using (statement.executeQuery()) {results => 
        util.bmap(results.next) { 
          process(results) match {
            case Success(v) => v
            case Failure(e) => error("Scoop Parse Error: " + e)
          }
        }
      }
    }
  } catch {
    case e: java.sql.SQLException => throw new Exception("SQL Exception ["+e.getMessage+"] when executing query ["+sql+"] with parameterss: ["+params+"]")
  }
  
  override def toString = {
    "Query(sql="+sql+", params=" + renderParams(params) +")"
  }
  
  //def apply ()(implicit con: Connection) = map(rs => rs.)
  //def single ()(implicit con: Connection) = singleOpt().get
  //def singleOpt ()(implicit con: Connection) = apply().toList.headOption
}

case class OrderBy (order: String, dir: String = null) {
  def sql = order + Option(dir).map(" "+_).getOrElse("")
}

/*
trait Predicate {
  def sql: String
  def params: List[Any]
}
*/
