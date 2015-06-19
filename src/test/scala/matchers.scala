import com.gravitydev.scoop._, query._

import org.scalatest.matchers.{Matcher, MatchResult}

object ScoopMatchers {
  class SqlMatcher (right: (String,Seq[SqlParam[_]]))(implicit dialect: SqlDialect) extends Matcher[ast.QueryNode] {
    // TODO: improve
    private def clean (s: String) = s.replaceAll("\\\n", " ").replaceAll("  ", " ").replaceAll("  ", " ").trim

    def apply(left: ast.QueryNode) = {
      val sql = dialect.toParameterizedSql(left)
      val l = (clean(sql.sql), sql.params.toList)
      val r = (clean(right._1), right._2.toList)

      MatchResult(
        l == r,
        "The SQL " + l + " does not match " + r,
        "The SQL " + left + " matches " + right
      )
    }
  }

  class SelectSqlMatcher (right: (String,Seq[SqlParam[_]]))(implicit dialect: SqlDialect) extends Matcher[ast.QueryNode] {
    // TODO: improve
    private def clean (s: String) = s.replaceAll("\\\n", " ").replaceAll("  ", " ").replaceAll("  ", " ").trim

    def apply(left: ast.QueryNode) = {
      val sql = dialect.toAliasedParameterizedSql(left)
      val l = (clean(sql.sql), sql.params)
      val r = (clean(right._1), right._2.toList)

      MatchResult(
        l == r,
        "The SQL " + l + " does not match " + r,
        "The SQL " + left + " matches " + right
      )
    }
  }

  def matchSql (sql: String, params: SqlParam[_]*) = new SqlMatcher(sql -> params.toSeq)
  def matchSelectSql (selectSql: String, params: SqlParam[_]*) = new SelectSqlMatcher(selectSql -> params.toSeq)
}
