package com.gravitydev.scoop
package util

import scala.collection.mutable.ListBuffer
import java.sql.{Connection, ResultSet, PreparedStatement}
import parsers.{ParseResult, ParseSuccess, ParseFailure}

private [scoop] class RsIterator[B](stmt: PreparedStatement, rowParser: ResultSet => ParseResult[B]) extends Iterator[B] {
  val rs = stmt.executeQuery()
  def hasNext: Boolean = {
    if (rs.next()) true else {
      rs.close()
      stmt.close()
      false
    }
  }
  def next(): B = rowParser(rs) match {
    case ParseSuccess(v) => v
    case ParseFailure(e) => sys.error("Scoop Parse Error: " + e)
  }
}

class QueryResult [T](iterator: Iterator[T]) {
  def list = iterator.toList

  /**
   * Converts to a stream
   * Be sure to consume the entire stream or ResultSet won't be closed
   */
  def stream = iterator.toStream

  def set = iterator.toSet
  def map [X] (fn: T=>X) = new QueryResult(iterator map fn)

  // force evaluation
  def head = iterator.toList.head
  def headOption = iterator.toList.headOption
  def isEmpty = iterator.toList.isEmpty
  def foreach (fn: T => Unit) = list foreach fn
}

private [scoop] object `package` {
  private type Closeable = {def close(): Unit}
  
  def bmap[T](test: => Boolean)(block: => T): List[T] = {
    val ret = new ListBuffer[T]
    while(test) ret += block
    ret.toList
  }

  def indent (s: String) = s.split("\n").map("  "+_).mkString("\n")

  // if multi-line, indent
  def formatSubExpr (s: String) = if (s contains "\n") "\n"+indent(s)+"\n" else s
  
  def using [T <: Closeable, R](closeable: T)(fn: T => R): R = 
    try fn(closeable) finally closeable.close()
    
    
  def processQuery [B](query: String)(fn: ResultSet => B)(implicit c: Connection): List[B] = {
    val stmt = c.prepareStatement(query)
    using (stmt) {statement =>
      using (statement.executeQuery()) {results => 
        bmap(results.next) { 
          fn(results)
        }
      }
    }
  }

  def inspectRS (rs: ResultSet) = {
    val md = rs.getMetaData
    val count = md.getColumnCount()

    def getColumns (remaining: Int): List[String] = {
      val idx = count - remaining + 1
      if (remaining == 0) Nil
      else (md.getColumnLabel(idx)+" ["+md.getColumnTypeName(idx)+"] (" + rs.getObject(idx) + ")") :: getColumns(remaining-1)
    }
    
    getColumns(count) mkString("ResultSet:\n", "\n", "\n")
  }

  def classToString (c: Class[_]) = c.getName.stripPrefix("java.lang.")

  def fnToString [P,T] (fn: _ => _) = {
    /* 
    // figure out the main 'apply' method to reflect
    val cls = fn.getClass
    val applyMethods = cls.getMethods.filter(_.getName == "apply")

    val nonAnyRefMethods = applyMethods.filter(_.getParameterTypes.head != classOf[AnyRef])

    val method = nonAnyRefMethods.headOption map {me =>
      cls.getMethod("apply", me.getParameterTypes.head)
    } getOrElse {
      cls.getMethod("apply", applyMethods.head.getParameterTypes.head)
    }
    
    val paramTypes = method.getParameterTypes

    classToString(paramTypes.head) + " => " + classToString(method.getReturnType)
    */ "Function"
  }
}

