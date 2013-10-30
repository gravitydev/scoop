import com.gravitydev.scoop._, query._

val query = using(issues, users as "reporter", users as "assignee") {(i, r, a) =>
  from(i)
  .innerJoin(r on i.reporter_id === r.id)
  .leftJoin(a on i.assigned_to === a.id)
  .where(r.accountId isNotNull)
  .orderBy(i.status desc, i.reason desc)
  .select(
    i.*,
    r.first_name as "reporter",
    a.first_name as "assignee",
    "(SELECT COUNT(*) FROM stats WHERE stats.issue_id = " + i.id.sql + ") as total_stats"
  )
}

