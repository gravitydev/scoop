package com.gravitydev.scoop.collection

sealed trait KList[+M[_], HL <: HList] {
  type HLIST = HL
  def toList: List[M[_]]
  
  def down (f: M ~> Id): HL
}

final case class KCons[H, T <: HList, +M[_]](head: M[H], tail: KList[M,T]) extends KList[M, H :+: T] {
  // prepend
  def :^: [N[X] >: M[X], G](g: N[G]) = KCons(g, this)

  def toList: List[M[_]] = head :: tail.toList
  
  def down (f: M ~> Id) = HCons(f(head), tail.down(f))
}

sealed class KNil extends KList[Nothing, HNil] {
  def :^: [M[_], H](h: M[H]) = KCons(h, this)
  
  def toList = Nil
  
  def down (f: Nothing ~> Id) = HNil
}

object KNil extends KNil

object KList {
  // nicer alias for pattern matching
  val :^: = KCons
}

