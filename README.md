Scoop
=====

Scoop is a query construction toolkit for scala. It is a very early experiment. 

Strong API
----------

*Not ready yet* The strong API is mostly modeled after squeryl (though nowhere near as featured at the moment), but it does not use case classes. 
You have to do your own mapping. The syntax is also just a bit closer to SQL since you can use symbols like >, <=, etc even with primitives.

```scala
import com.gravitydev.scoop._, strong._

val query = from(users)(u => 
  where(u.age > 24 and u.name === "alvaro")
  select(u.first_name, u.last_name)
)
```

Query API (stringly typed)
--------------------------

*Mostly usable* This API sacrifices some safety for flexibility and in some cases readability. It reads a lot closer to actual SQL.

```scala
import com.gravitydev.scoop._, query._

val i = issues
val r = users as "reporter"
val a = users as "assignee"

val query = from(issues)
  .innerJoin(r on i.reporter_id === r.id)
  .leftJoin(a on i.assigned_to === a.id)
  .where(r.accountId isNotNull)
  .orderBy(i.status desc, i.reason desc)
  .select(i.*, r.first_name as "reporter", a.first_name as "assignee")
```

Installation
------------

TODO

Acknowledgements
----------------

Lots of inspiration from squeryl, anorm, and the apocalisp blog.

