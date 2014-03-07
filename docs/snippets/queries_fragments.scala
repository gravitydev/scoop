// re-usable predicates
// use on any query as long as you have an alias for table.users
def userActive (u: tables.users): Predicate = 
  u.activated_at.isNotNull && u.deleted === false

def userMatch (u: tables.users, str: String): Predicate =
  (u.first_name like str) || (u.last_name like str)

...
// same table, self-join, different aliases
using (tables.users, tables.users) {(employee, manager) =>
val users = 
  from(employee)
    .innerJoin(manager on employee.manager_id === manager.id)
    .where(
      userActive(manager) &&
      userMatch(employee, "James%")
    )
    .find(...)
