import com.gravitydev.scoop._, query._

import org.scalatest.matchers.{Matcher, MatchResult}

object ScoopMatchers {
  class SqlMatcher (right: (String,Seq[SqlParam[_]])) extends Matcher[{def sql: String; def params: Seq[SqlParam[_]]}] {
    // TODO: improve
    private def clean (s: String) = s.replaceAll("\\\n", " ").replaceAll("  ", " ").replaceAll("  ", " ").trim

    def apply(left: {def sql: String; def params: Seq[SqlParam[_]]}) = {
      val l = (clean(left.sql), left.params.toList)
      val r = (clean(right._1), right._2.toList)

      MatchResult(
        l == r,
        "The SQL " + l + " does not match " + r,
        "The SQL " + left + " matches " + right
      )
    }
  }

  def matchSql (sql: String, params: SqlParam[_]*) = new SqlMatcher(sql -> params.toSeq)
}
