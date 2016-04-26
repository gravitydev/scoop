package com.gravitydev.scoop
package definition

object `package` {
  // convenience
  type TableT = Table[T] forSome {type T <: Table[T]}
}

/**
 * Represents an SQL table
 *
 * @param constructor A function to get an instance of the table (companion of a case class should work)
 * @param name Name of the table
 */
abstract class Table [T <: Table[T]](constructor: () => T, name: String, schema: Option[String]=None) {
  // prefixed to prevent conflicts with column names
  val _tableName = name
  val _schema = schema

  /* There's no good way to expose this without using a var */
  private[definition] var __alias: Option[String] = None
  private[scoop] def _alias = __alias
  def as (newAlias: String) = {
    val t = constructor()
    t.__alias = Some(newAlias)
    t
  }

  def col[T:SqlType](name: Symbol) = NonNullableColumn[T](name.name, this)
  def col[T:SqlType](name: String) = NonNullableColumn[T](name, this)

  implicit class NonNullableColumnOps [T:SqlType](col: NonNullableColumn[T]) {
    /** Convert a non-nullable column to a nullable column (part of definition DSL) */
    def nullable = NullableColumn[T](col.name, col.table)
  }
}

sealed abstract class Column[T:SqlType] { //extends SqlExpr[T] {
  val sqlTpe = ast.SqlType[T]
  def name: String
  def table: TableT
}

case class NonNullableColumn [T:SqlType] (name: String, table: TableT) extends Column[T] //with ast.SqlParseStrictExpr[T] with ast.SqlNamedExpr[T,T] with Selection[T] 

case class NullableColumn [T:SqlType] (name: String, table: TableT) extends Column[T] //with ast.SqlParseOptExpr[T] with ast.SqlNamedExpr[T,Option[T]] with Selection[Option[T]]


/*
abstract class Table [T <: Table[T]](_companion: TableCompanion[T], tableName: String = null, schema: String = null) {self: T =>
  // hmm... this is a bit brittle, but it *is* convenient
  val _tableName: String = Option(tableName) getOrElse _companion.getClass.getName.split('.').last.split('$').last

  val _schema = Option(schema)
  
  // Mutable for convenience
  // should only be changed by scoop
  private var __alias = _tableName
  
  def _alias = __alias
  def _prefix = _alias + "_" 

  implicit def _self = this

  def col[T:SqlType](name: Symbol, cast: String = null) = new ast.SqlNonNullableCol[T](name.name, Option(cast), this)
  
  def as (alias: String): T = {
    val t = _companion.apply
    t.__alias = alias
    t
  }
  
  // so it can serve as a companion
  def apply (): T = this

  // generate a column alias
  def apply [X:SqlType](column: String) = ast.SqlRawExpr[X](_alias+"."+column)
  
  def fromSql = if (_tableName == _alias) _tableName else _tableName + " as " + _alias
  def updateSql = _tableName
 
  override def toString = "SqlTable(" + fromSql + ")"
}
*/



/*
sealed abstract class Column[T:SqlType] (val cast: Option[String], val table: Table[_], explicitAlias: String = null) extends SqlExpr[T] {
  val sqlTpe = SqlType[T]

  def columnName: String

  def name: String = Option(explicitAlias) getOrElse table._prefix + columnName

  val params = Nil

  def expressions: Seq[ast.SqlNamedExpr[_,_]] // = List( new query.SelectExprS(sql + " as " + name) )
}
*/

/*
class SqlNonNullableCol[T:SqlType](val columnName: String, cast: Option[String], table: SqlTable[_], explicitAlias: String = null) 
    extends SqlColumn[T] (cast, table, explicitAlias) with SqlParseStrictExpr[T] with SqlNamedExpr[T,T] with Selection[T] {

  override def toString = "Col(" + columnName + " as " + name + ")"

  def parse (rs: ResultSet): Option[T] = SqlType[T].parse(rs, name)

  def nullable = new SqlNullableCol[T](columnName, cast, table, explicitAlias)
  def := (x: SqlExpr[T]) = SqlAssignment(this, x)
}

class SqlNullableCol[T:SqlType](val columnName: String, cast: Option[String], table: SqlTable[_], explicitAlias: String) 
    extends SqlCol[T] (cast, table, explicitAlias) with SqlParseOptExpr[T] with SqlNamedExpr[T,Option[T]] with Selection[Option[T]] {

  //override def toString = "Col("+ columnName + " as " + name +" : Nullable)"

  def parse (rs: ResultSet): Option[Option[T]] = Some(SqlType[T].parse(rs, name))

  def := (x: Option[SqlExpr[T]]) = SqlAssignment(this, x getOrElse SqlRawExpr[T]("NULL"))
}
*/

