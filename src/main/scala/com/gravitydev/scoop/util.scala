package com.gravitydev.scoop.util

import scala.collection.mutable.ListBuffer
import java.sql.{Connection, ResultSet}

object `package` {
  private type Closeable = {def close(): Unit}
    
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
}
