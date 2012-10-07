package com.gravitydev.scoop

package object collection {
  val :^: = KCons
  val :+: = HCons
  type :+:[H, T <: HList] = HCons[H,T]
  
  /** Natural transformation */
  trait ~>[-F[_],+G[_]] {
    def apply[A](a: F[A]): G[A]
  }

  type Id[A] = A
}
