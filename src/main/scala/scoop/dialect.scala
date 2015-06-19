package com.gravitydev.scoop

import org.kiama.output.PrettyPrinter
import com.gravitydev.scoop.query.ParameterizedSql

trait SqlDialect extends PrettyPrinter {
  def sql (n: ast.QueryNode): Doc
  def aliasedSql (n: ast.QueryNode): Doc
  def params (n: ast.QueryNode): Seq[SqlParam[_]]

  def toParameterizedSql (q: ast.QueryNode): query.ParameterizedSql = query.ParameterizedSql(formattedSql(q), params(q))
  def toAliasedParameterizedSql (q: ast.QueryNode): query.ParameterizedSql = query.ParameterizedSql(formattedAliasedSql(q), params(q)) 
 
  def formattedSql (q: ast.QueryNode) = pretty(sql(q), 80).layout
  def formattedAliasedSql (q: ast.QueryNode) = pretty(aliasedSql(q), 80).layout
}

trait BaseSqlDialect extends SqlDialect {
  override val defaultIndent = 2

  def opt (o: Option[Doc]) = o getOrElse empty

  def csv (s: Seq[Doc]) = nest(vsep(s.toList, comma))

  def optSeq (prefix: Doc, n: Seq[Doc]) = if (n.isEmpty) empty else prefix <+> csv(n) <> line
 
  def sql (n: ast.QueryNode): Doc = n match {
    case q: ast.Query[_] => 
      opt(q.comment.map(_ <> line)) <>
      // select
      "SELECT" <+> csv(q.sel.expressions map aliasedSql) <>
      // from
      opt( q.from.map(queryable => line <> "FROM" <+> aliasedSql(queryable) ) ) <> 
      // joins
      (if (q.joins.nonEmpty) line else empty) <> vsep( q.joins.toList.map(sql(_)) ) <> 
      // where
      opt( q.predicate.map(w => line <> "WHERE" <+> nest(sql(w)) ) ) <>
      // group by
      optSeq(
        line <> "GROUP BY",
         q.group.toList.map(sql)
      ) <>
      // order by
      optSeq(
        line <> "ORDER BY",
        q.order.toList.map(sql)
      ) <>
      // limit
      opt( q.limit.map(l => line <> "LIMIT" <+> l.toString) ) <>
      // offset
      opt( q.offset.map(o => line <> "OFFSET" <+> o.toString) ) <>
      // update lock
      opt( Option(line <> "FOR UPDATE").filter(_ => q.forUpdateLock) )

    case c: ast.SqlCol[_] => c.table._alias <> "." <> c.columnName

    case ast.SqlNamedStrictExpr(expr,_) => sql(expr)

    case ast.SqlInfixExpr(l,r,op) => sql(l) <+> op <+> sql(r)

    case ast.SqlBinaryApplic(values, op) =>
      ssep( 
        values.toList.map {
          case x @ ast.SqlBinaryApplic(_,_) => parens( nest(line <> sql(x)) <> line )
          case x => sql(x)
        }, 
        " " <> op <> line 
      )
  
    case ast.SqlLiteralExpr(_) => "?"

    case ast.SqlAssignment(col, value) => col.columnName <+> "=" <+> (sql(value) <> string(col.cast.map("::" + _).getOrElse(""))) // postgres hack

    case ast.Join(queryable, onPred, joinType) => joinType.sql <+> "JOIN" <+> aliasedSql(queryable) <+> "ON" <+> sql(onPred)

    case ast.Delete(table, pred, comment) => 
      comment.map("/*" <+> _ <+> "*/").getOrElse(empty) <> line <> 
      "DELETE FROM" <+> table._tableName <> line <>
      "WHERE" <+> sql(pred)

    case ast.Insert(table, assignments, comment) => 
      comment.map("/*" <+> _ <+> "*/").getOrElse(empty) <> line <> 
      "INSERT INTO" <+> table <+> 
      parens( ssep( assignments.toList.map(_.col.columnName).map(string), comma <> " ") ) <+>
      "VALUES" <+>
      parens( ssep( assignments.toList.map(_ => string("?")), comma <> " ") )

    case ast.Update (table, assignments, predicate, comment) =>
      comment.map("/*" <+> _ <+> "*/").getOrElse(empty) <> line <> 
      "UPDATE" <+> table._tableName <+> "SET" <+> 
      ssep( 
        assignments.toList.map(sql),
        comma <> " "
      ) <> line <>
      opt( predicate.map(w => "WHERE" <+> sql(w) <> line) )

    case ast.SqlUnaryExpr(l, op, postfix) => if (postfix) sql(l) <+> op else op <+> sql(l)

    case ast.SqlRawExpr(expr) => string(expr.sql)

    case ast.Upsert(insert, assignments) => sql(insert) <> line <> "ON DUPLICATE KEY UPDATE" <+> ssep( assignments.toList.map(sql), comma <> " ")

    case ast.SqlOrdering(expr, dir) => sql(expr) <+> dir.sql

    case ast.SqlQueryExpr(query) => parens( nest(line <> sql(query)) <> line )

    case ast.SqlWrappedExpr(expr) => sql(expr)
  }

  def params (q: ast.QueryNode): Seq[SqlParam[_]] = q match {
    case q: builder.Query[_] =>
      // select
      q.sel.expressions.flatMap(params) ++
      // from
      q.from.toList.flatMap(params) ++
      // joins
      q.joins.flatMap(params) ++
      // where
      q.predicate.toList.flatMap(params) ++
      // group by
      q.group.flatMap(params) ++
      // order by
      q.order.flatMap(params) 
    case t: ast.TableT => Nil
    case ast.SqlNamedStrictExpr(expr,_) => params(expr)
    case ast.SqlInfixExpr(l,r,_) => params(l) ++ params(r)
    case ast.SqlBinaryApplic(values, op) => values.flatMap(params)
    case l @ ast.SqlLiteralExpr(_) => Seq(l.toParam)
    case col: ast.SqlCol[_] => Nil
    case ast.SqlAssignment(_, value) => params(value) 
    case ast.Join(queryable, onPred, _) => params(queryable) ++ params(onPred)
    case ast.Delete(table, pred, _) => params(pred)
    case ast.Insert(_, assignments, _) => assignments.flatMap(params)
    case ast.Update (_, assignments, predicate, _) => assignments.flatMap(params) ++ predicate.toList.flatMap(params)
    case ast.SqlUnaryExpr(l,_,_) => params(l) 
    case ast.SqlRawExpr(expr) => expr.params
    case ast.Upsert(insert, assignments) => params(insert) ++ assignments.toList.flatMap(params)
    case ast.SqlOrdering(expr, _) => params(expr) 
    case ast.SqlQueryExpr(query) => params(query)
    case ast.SqlWrappedExpr(expr) => params(expr)
  }

  def aliasedSql (n: ast.QueryNode): Doc = n match {
    case ast.SqlNamedStrictExpr(expr @ ast.SqlInfixExpr(_,_,_), name) => parens( sql(expr) ) <+> "as" <+> name
    case x @ ast.SqlNamedQueryExpr(query, name) => parens( sql(query) ) <+> "as" <+> name
    case x @ ast.SqlNamedStrictExpr(expr, name) => sql(expr) <+> "as" <+> name
    case col: ast.SqlCol[_] => sql(col) <+> "as" <+> col.name
    case t: ast.TableT => t.fromSql  
  }
}

object BaseSqlDialect extends BaseSqlDialect

object PostgresDialect extends BaseSqlDialect {
  override def sql (q: ast.QueryNode): Doc = q match {
    case c: ast.SqlCol[_] => c.table._alias + "." + c.columnName + c.cast.map(_=>"::varchar").getOrElse("") // TODO: use correct base type
    case _ => super.sql(q)
  }
}

