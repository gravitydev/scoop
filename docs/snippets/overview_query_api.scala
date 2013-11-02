import com.gravitydev.scoop._, query._

case class Issue(
  id: Long, title: String, status: Status, 
  reporter: User, 
  assignee: Option[User]
)
case class User(id: Long, name: String)

val issues: List[Issue] = using(issues, users, users) {(i, reporter, assignee) =>
  from(i)
  .innerJoin(reporter on i.reporter_id === reporter.id)
  .leftJoin(assignee on i.assigned_to === assignee.id)
  .where(i.status === Open && i.reported_at >= DateTime.now.minusDays(7))
  .orderBy(i.status desc, i.reason desc)
  .find(
    i.id ~ i.status ~ i.reason ~ 
    (reporter.id ~ reporter.name >> User.apply)
    opt(assignee.id ~ assignee.name >> User.apply) >> Issue.apply
  )
}

