import com.gravitydev.scoop._, query._

import org.scalatest.matchers.{Matcher, MatchResult}

object ScoopMatchers {
  class SqlMatcher (right: (String,Seq[SqlParam[_]])) extends Matcher[{def sql: String; def params: Seq[SqlParam[_]]}] {
    def apply(left: {def sql: String; def params: Seq[SqlParam[_]]}) = {
      MatchResult(
        (left.sql, left.params) == right,
        "The SQL " + left + " does not match " + right,
        "The SQL " + left + " matches " + right
      )
    }
  }

  def matchSql (sql: String, params: SqlParam[_]*) = new SqlMatcher(sql -> params.toSeq)
}
