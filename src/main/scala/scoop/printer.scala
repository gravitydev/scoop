package com.gravitydev.scoop
package printer

import org.kiama.output.PrettyPrinter
import com.gravitydev.scoop.query.SqlS

object Printer extends PrettyPrinter {
  override val defaultIndent = 2

  def opt (o: Option[Doc]) = o getOrElse empty

  def csv (s: Seq[Doc]) = nest(vsep(s.toList, comma))

  def optSeq (prefix: Doc, n: Seq[Doc]) = if (n.isEmpty) empty else prefix <+> csv(n) <> line
 
  def showQuery (q: builder.Query): Doc = 
    opt(q.comment.map(_ <> line)) <>
    // select
    "SELECT" <+> csv(q.sel.toList map showSelExpr) <@>
    // from
    opt( q.from.map(_.sql <> line) ) <> 
    // joins
    vsep( q.joins.toList.map(_.sql <> line) ) <>
    // where
    opt( q.predicate.map(w => "WHERE" <+> w.sql <> line) ) <>
    // group by
    optSeq(
      "GROUP BY",
       q.group.toList.map(g => string(g.sql))
    ) <>
    // order by
    optSeq(
      "ORDER BY",
      q.order.toList.map(g => string(g.sql))
    ) <>
    // limit
    opt( q.limit.map(l => "LIMIT" <+> l.toString <> line) ) <>
    // offset
    opt( q.offset.map(o => "OFFSET" <+> o.toString <> line) ) <>
    // update lock
    opt( Option("FOR UPDATE" <> line).filter(_ => q.forUpdateLock) )

  def showSelExpr (x: builder.SelectExpr): Doc = x.sql
 
  def getSql (q: builder.Query) = pretty(showQuery(q), 20).layout.trim

  def getParams (q: builder.Query) = 
    // select
    q.sel.flatMap(_.params) ++
    // from
    q.from.toList.flatMap(_.params) ++
    // joins
    q.joins.flatMap(_.params) ++
    // where
    q.predicate.toList.flatMap(_.params) ++
    // group by
    q.group.flatMap(_.params) ++
    // order by
    q.order.flatMap(_.params) 

  def toRawQuery (q: builder.Query): query.RawQuery = new query.RawQuery(getSql(q), getParams(q))
}

  /*
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
    limitSql +~
    offsetSql +~
    (ifStr(forUpdateLock)("FOR UPDATE \n"))
  */
 
  /* 
  lazy val sql: String = {
    //statement.sql
    ???
  }
  */
