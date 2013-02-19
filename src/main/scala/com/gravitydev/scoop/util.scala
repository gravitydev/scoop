package com.gravitydev.scoop.util

import scala.collection.mutable.ListBuffer
import java.sql.{Connection, ResultSet}
import org.slf4j.{Logger, LoggerFactory}

object `package` {
  private type Closeable = {def close(): Unit}
  
  // alias logging
  class ScoopLogger (underlying: Logger) {
    def debug (msg: => String) = underlying.debug(msg)
    def info (msg: => String) = underlying.info(msg)
    def warn (msg: => String) = underlying.warn(msg)
    def error (msg: => String) = underlying.error(msg)
  }

  trait Logging {
    lazy val logger = new ScoopLogger(LoggerFactory getLogger getClass)
  }
    
  def bmap[T](test: => Boolean)(block: => T): List[T] = {
    val ret = new ListBuffer[T]
    while(test) ret += block
    ret.toList
  }
  
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

