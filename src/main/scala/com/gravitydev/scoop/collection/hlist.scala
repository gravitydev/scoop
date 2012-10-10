package com.gravitydev.scoop.collection

sealed trait HList {
  type Appended[L] <: HList
  def append[L](l: L) :Appended[L]

  def toList: List[_] = this match {
    case h: HCons[_,_] => h.head :: h.tail.toList
    case h: HNil => Nil
  }
}

final case class HCons [H, T<:HList] (head: H, tail: T) extends HList {
  type Appended[L] = HCons[H, tail.Appended[L]]

  def ::[C](c: C): HCons[C, HCons[H, T]] = HCons(c, this)
  def append[L](l: L) = HCons(head, tail.append(l))

  // too lazy to implement folds
  override def toString = toList.mkString("HList(",", ",")")
}

sealed abstract class HNil extends HList {
  type Appended[L] = HCons[L, HNil]

  def ::[C](c: C): HCons[C, HNil] = HCons(c, this)
  def append[L](l: L): Appended[L] = l :: HNil

  override def toString = "HNil"
}
object HNil extends HNil
