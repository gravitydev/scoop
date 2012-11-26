package com.gravitydev.scoop
package boilerplate

import com.gravitydev.scoop.collection._
import java.sql.ResultSet

object `package` {
  /*
  // this really should be in the std lib
  def tuple [A,B](a:A,b:B) = (a,b)
  def tuple [A,B,C](a:A,b:B,c:C) = (a,b,c)
  def tuple [A,B,C,D](a:A,b:B,c:C,d:D) = (a,b,c,d)
  def tuple [A,B,C,D,E](a:A,b:B,c:C,d:D,e:E) = (a,b,c,d,e)
  */

  def hlist2 [A,B] (a:A,b:B) = a :: b :: HNil 
  def hlist3 [A,B,C] (a:A,b:B,c:C) = a :: b :: c :: HNil
  def hlist4 [A,B,C,D](a:A,b:B,c:C,d:D) = a :: b :: c :: d :: HNil
  def hlist5 [A,B,C,D,E](a:A,b:B,c:C,d:D,e:E) = a :: b :: c :: d :: e :: HNil
  def hlist6 [A,B,C,D,E,F](a:A,b:B,c:C,d:D,e:E,f:F) = a :: b :: c :: d :: e :: f :: HNil
  def hlist7 [A,B,C,D,E,F,G](a:A,b:B,c:C,d:D,e:E,f:F,g:G) = a :: b :: c :: d :: e :: f :: g :: HNil
  def hlist8 [A,B,C,D,E,F,G,H](a:A,b:B,c:C,d:D,e:E,f:F,g:G,h:H) = a :: b :: c :: d :: e :: f :: g :: h :: HNil
  def hlist9 [A,B,C,D,E,F,G,H,I](a:A,b:B,c:C,d:D,e:E,f:F,g:G,h:H,i:I) = a :: b :: c :: d :: e :: f :: g :: h :: i :: HNil
  def hlist10 [A,B,C,D,E,F,G,H,I,J](a:A,b:B,c:C,d:D,e:E,f:F,g:G,h:H,i:I,j:J) = a :: b :: c :: d :: e :: f :: g :: h :: i :: j :: HNil
  def hlist11 [A,B,C,D,E,F,G,H,I,J,K](a:A,b:B,c:C,d:D,e:E,f:F,g:G,h:H,i:I,j:J,k:K) = a :: b :: c :: d :: e :: f :: g :: h :: i :: j :: k :: HNil
  def hlist12 [A,B,C,D,E,F,G,H,I,J,K,L](a:A,b:B,c:C,d:D,e:E,f:F,g:G,h:H,i:I,j:J,k:K,l:L) = a :: b :: c :: d :: e :: f :: g :: h :: i :: j :: k :: l :: HNil
  def hlist13 [A,B,C,D,E,F,G,H,I,J,K,L,M](a:A,b:B,c:C,d:D,e:E,f:F,g:G,h:H,i:I,j:J,k:K,l:L,m:M) = a :: b :: c :: d :: e :: f :: g :: h :: i :: j :: k :: l :: m :: HNil
  def hlist14 [A,B,C,D,E,F,G,H,I,J,K,L,M,N](a:A,b:B,c:C,d:D,e:E,f:F,g:G,h:H,i:I,j:J,k:K,l:L,m:M,n:N) = a :: b :: c :: d :: e :: f :: g :: h :: i :: j :: k :: l :: m :: n :: HNil
  def hlist15 [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](a:A,b:B,c:C,d:D,e:E,f:F,g:G,h:H,i:I,j:J,k:K,l:L,m:M,n:N,o:O) = a :: b :: c :: d :: e :: f :: g :: h :: i :: j :: k :: l :: m :: n :: o :: HNil
  def hlist16 [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q](a:A,b:B,c:C,d:D,e:E,f:F,g:G,h:H,i:I,j:J,k:K,l:L,m:M,n:N,o:O,q:Q) = a :: b :: c :: d :: e :: f :: g :: h :: i :: j :: k :: l :: m :: n :: o :: q :: HNil
  def hlist17 [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q,R](a:A,b:B,c:C,d:D,e:E,f:F,g:G,h:H,i:I,j:J,k:K,l:L,m:M,n:N,o:O,q:Q,r:R) = a::b::c::d::e::f::g::h::i::j::k::l::m::n::o::q::r::HNil
  def hlist18 [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q,R,S](a:A,b:B,c:C,d:D,e:E,f:F,g:G,h:H,i:I,j:J,k:K,l:L,m:M,n:N,o:O,q:Q,r:R,s:S) = a::b::c::d::e::f::g::h::i::j::k::l::m::n::o::q::r::s::HNil
  def hlist19 [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q,R,S,T](a:A,b:B,c:C,d:D,e:E,f:F,g:G,h:H,i:I,j:J,k:K,l:L,m:M,n:N,o:O,q:Q,r:R,s:S,t:T) = a::b::c::d::e::f::g::h::i::j::k::l::m::n::o::q::r::s::t::HNil
  def hlist20 [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q,R,S,T,U](a:A,b:B,c:C,d:D,e:E,f:F,g:G,h:H,i:I,j:J,k:K,l:L,m:M,n:N,o:O,q:Q,r:R,s:S,t:T,u:U) = a::b::c::d::e::f::g::h::i::j::k::l::m::n::o::q::r::s::t::u::HNil
  def hlist21 [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q,R,S,T,U,V](a:A,b:B,c:C,d:D,e:E,f:F,g:G,h:H,i:I,j:J,k:K,l:L,m:M,n:N,o:O,q:Q,r:R,s:S,t:T,u:U,v:V) = a::b::c::d::e::f::g::h::i::j::k::l::m::n::o::q::r::s::t::u::v::HNil
  def hlist22 [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q,R,S,T,U,V,W](a:A,b:B,c:C,d:D,e:E,f:F,g:G,h:H,i:I,j:J,k:K,l:L,m:M,n:N,o:O,q:Q,r:R,s:S,t:T,u:U,v:V,w:W) = a::b::c::d::e::f::g::h::i::j::k::l::m::n::o::q::r::s::t::u::v::w::HNil

  private [boilerplate] type P[T] = ResultSetParser[T]

}

// TODO: sigh, i really need to figure out how to remove this boilerplate with KLists
/*
class CompoundParser [H, T <: HList](parsers: KCons[H, T, ResultSetParser]) {
  def ~ [X] (px: P[X]) = new CompoundParser(px :^: parsers)
}
*/

abstract class ParserBase [A] (fn: ResultSet => ParseResult[A]) extends P[A] {
  def ~ [X](px: P[X]) = new Parser2(this, px)
  def apply (rs: ResultSet) = fn(rs)
  def columns: List[query.SelectExprS]
  def >> [T](fn: A=>T) = new Parser1(this map fn)
}

class CompoundParser [A,X](parser: ParserX[X], mapped: P[A]) extends P[A] {
  def columns = parser.columns
  def apply (rs: ResultSet) = mapped(rs)
  def ~ [X](px: P[X]) = new Parser2(this, px)
  def >> [T](fn: A=>T) = new Parser1(this map fn)
}

abstract class ParserX [T] extends ResultSetParser [T] {
  def list: List[ResultSetParser[_]]
  def columns = list.foldLeft(List[query.SelectExprS]())((l,p) => l ++ p.columns)
}

class Parser1 [A](pa: P[A]) extends ParserX[A] {
  def list = List(pa)
  def >> [T](fn: A=>T) = new Parser1(pa map fn)
  def ~ [X](px: P[X]) = new Parser2(pa,px)
  def apply (rs: ResultSet) = pa(rs)
}

class Parser2 [A,B](pa: P[A], pb: P[B]) extends ParserX[A :+: B :+: HNil] {
  def list = List(pa,pb)
  def >> [T](fn: (A,B)=>T) = new CompoundParser(this, for (a <- pa; b <- pb) yield fn(a,b))
  def ~ [X](px: P[X]) = new Parser3(pa,pb,px)
  def apply (rs: ResultSet) = >>(hlist2)(rs)
}

class Parser3 [A,B,C](pa: P[A], pb: P[B], pc: P[C]) extends ParserX[A:+:B:+:C:+:HNil] {
  def list = List(pa,pb,pc)
  def >> [T](fn: (A,B,C)=>T) = new CompoundParser(this, for (a <- pa; b <- pb; c <- pc) yield fn(a,b,c))
  def ~ [X](px: P[X]) = new Parser4(pa,pb,pc,px)
  def apply (rs: ResultSet) = >>(hlist3)(rs)
}

class Parser4 [A,B,C,D](pa: P[A], pb: P[B], pc: P[C], pd: P[D]) extends ParserX[A:+:B:+:C:+:D:+:HNil] {
  def list = List(pa,pb,pc,pd)
  def >> [T](fn: (A,B,C,D)=>T) = new CompoundParser(this, for (a <- pa; b <- pb; c <- pc; d <- pd) yield fn(a,b,c,d))
  def ~ [X](px: P[X]) = new Parser5(pa,pb,pc,pd,px)
  def apply (rs: ResultSet) = >>(hlist4)(rs)
}

class Parser5 [A,B,C,D,E](pa: P[A], pb: P[B], pc: P[C], pd: P[D], pe: P[E]) extends ParserX[A:+:B:+:C:+:D:+:E:+:HNil]{
  def list = List(pa,pb,pc,pd,pe)
  def >> [T](fn: (A,B,C,D,E)=>T) = new CompoundParser(this, for (a <- pa; b <- pb; c <- pc; d <- pd; e <- pe) yield fn(a,b,c,d,e))
  def ~ [X](px: P[X]) = new Parser6(pa,pb,pc,pd,pe,px)
  def apply (rs: ResultSet) = >>(hlist5)(rs)
}

class Parser6 [A,B,C,D,E,F](pa: P[A], pb: P[B], pc: P[C], pd: P[D], pe: P[E], pf: P[F]) extends ParserX[A:+:B:+:C:+:D:+:E:+:F:+:HNil]{
  def list = List(pa,pb,pc,pd,pe,pf)
  def >> [T](fn: (A,B,C,D,E,F)=>T) = new CompoundParser(this, for (a <- pa; b <- pb; c <- pc; d <- pd; e <- pe; f <- pf) yield fn(a,b,c,d,e,f))
  def ~ [X](px: P[X]) = new Parser7(pa,pb,pc,pd,pe,pf,px)
  def apply (rs: ResultSet) = >>(hlist6)(rs)
}

class Parser7 [A,B,C,D,E,F,G](pa: P[A], pb: P[B], pc: P[C], pd: P[D], pe: P[E], pf: P[F], pg: P[G]) extends ParserX[A:+:B:+:C:+:D:+:E:+:F:+:G:+:HNil]{
  def list = List(pa,pb,pc,pd,pe,pf,pg)
  def >> [T](fn: (A,B,C,D,E,F,G)=>T) = new CompoundParser(this, for (a <- pa; b <- pb; c <- pc; d <- pd; e <- pe; f <- pf; g <- pg) yield fn(a,b,c,d,e,f,g))
  def ~ [X](px: P[X]) = new Parser8(pa,pb,pc,pd,pe,pf,pg,px)
  def apply (rs: ResultSet) = >>(hlist7)(rs)
}

class Parser8 [A,B,C,D,E,F,G,H](pa: P[A], pb: P[B], pc: P[C], pd: P[D], pe: P[E], pf: P[F], pg: P[G], ph: P[H]) extends ParserX[A:+:B:+:C:+:D:+:E:+:F:+:G:+:H:+:HNil]{
  def list = List(pa,pb,pc,pd,pe,pf,pg,ph)
  def >> [T](fn: (A,B,C,D,E,F,G,H)=>T) = new CompoundParser(this, for (a <- pa; b <- pb; c <- pc; d <- pd; e <- pe; f <- pf; g <- pg; h <- ph) yield fn(a,b,c,d,e,f,g,h))
  def ~ [X](px: P[X]) = new Parser9(pa,pb,pc,pd,pe,pf,pg,ph,px)
  def apply (rs: ResultSet) = >>(hlist8)(rs)
}

class Parser9 [A,B,C,D,E,F,G,H,I](pa: P[A], pb: P[B], pc: P[C], pd: P[D], pe: P[E], pf: P[F], pg: P[G], ph: P[H], pi: P[I]) extends ParserX[A:+:B:+:C:+:D:+:E:+:F:+:G:+:H:+:I:+:HNil]{
  def list = List(pa,pb,pc,pd,pe,pf,pg,ph,pi)
  def >> [T](fn: (A,B,C,D,E,F,G,H,I)=>T) = new CompoundParser(this, for (a <- pa; b <- pb; c <- pc; d <- pd; e <- pe; f <- pf; g <- pg; h <- ph; i <- pi) yield fn(a,b,c,d,e,f,g,h,i))
  def ~ [X](px: P[X]) = new Parser10(pa,pb,pc,pd,pe,pf,pg,ph,pi,px)
  def apply (rs: ResultSet) = >>(hlist9)(rs)
}

class Parser10 [A,B,C,D,E,F,G,H,I,J](pa: P[A], pb: P[B], pc: P[C], pd: P[D], pe: P[E], pf: P[F], pg: P[G], ph: P[H], pi: P[I], pj: P[J]) extends ParserX[A:+:B:+:C:+:D:+:E:+:F:+:G:+:H:+:I:+:J:+:HNil]{
  def list = List(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj)
  def >> [T](fn: (A,B,C,D,E,F,G,H,I,J)=>T) = new CompoundParser(this, for (a <- pa; b <- pb; c <- pc; d <- pd; e <- pe; f <- pf; g <- pg; h <- ph; i <- pi; j <- pj) yield fn(a,b,c,d,e,f,g,h,i,j))
  def ~ [X](px: P[X]) = new Parser11(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj,px)
  def apply (rs: ResultSet) = >>(hlist10)(rs)
}

class Parser11 [A,B,C,D,E,F,G,H,I,J,K](pa: P[A], pb: P[B], pc: P[C], pd: P[D], pe: P[E], pf: P[F], pg: P[G], ph: P[H], pi: P[I], pj: P[J], pk: P[K]) extends ParserX[A:+:B:+:C:+:D:+:E:+:F:+:G:+:H:+:I:+:J:+:K:+:HNil]{
  def list = List(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj,pk)
  def >> [T](fn: (A,B,C,D,E,F,G,H,I,J,K)=>T) = new CompoundParser(this, for (a <- pa; b <- pb; c <- pc; d <- pd; e <- pe; f <- pf; g <- pg; h <- ph; i <- pi; j <- pj; k <- pk) yield fn(a,b,c,d,e,f,g,h,i,j,k))
  def ~ [X](px: P[X]) = new Parser12(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj,pk,px)
  def apply (rs: ResultSet) = >>(hlist11)(rs)
}

class Parser12 [A,B,C,D,E,F,G,H,I,J,K,L](pa: P[A], pb: P[B], pc: P[C], pd: P[D], pe: P[E], pf: P[F], pg: P[G], ph: P[H], pi: P[I], pj: P[J], pk: P[K], pl: P[L]) extends ParserX[A:+:B:+:C:+:D:+:E:+:F:+:G:+:H:+:I:+:J:+:K:+:L:+:HNil]{
  def list = List(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj,pk,pl)
  def >> [T](fn: (A,B,C,D,E,F,G,H,I,J,K,L)=>T) = new CompoundParser(this, for (a <- pa; b <- pb; c <- pc; d <- pd; e <- pe; f <- pf; g <- pg; h <- ph; i <- pi; j <- pj; k <- pk; l <- pl) yield fn(a,b,c,d,e,f,g,h,i,j,k,l))
  def ~ [X](px: P[X]) = new Parser13(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj,pk,pl,px)
  def apply (rs: ResultSet) = >>(hlist12)(rs)
}

class Parser13 [A,B,C,D,E,F,G,H,I,J,K,L,M](pa: P[A], pb: P[B], pc: P[C], pd: P[D], pe: P[E], pf: P[F], pg: P[G], ph: P[H], pi: P[I], pj: P[J], pk: P[K], pl: P[L], pm: P[M]) extends ParserX[A:+:B:+:C:+:D:+:E:+:F:+:G:+:H:+:I:+:J:+:K:+:L:+:M:+:HNil]{
  def list = List(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj,pk,pl,pm)
  def >> [T](fn: (A,B,C,D,E,F,G,H,I,J,K,L,M)=>T) = new CompoundParser(this, for (a <- pa; b <- pb; c <- pc; d <- pd; e <- pe; f <- pf; g <- pg; h <- ph; i <- pi; j <- pj; k <- pk; l <- pl; m <- pm) yield fn(a,b,c,d,e,f,g,h,i,j,k,l,m))
  def ~ [X](px: P[X]) = new Parser14(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj,pk,pl,pm,px)
  def apply (rs: ResultSet) = >>(hlist13)(rs)
}

class Parser14 [A,B,C,D,E,F,G,H,I,J,K,L,M,N](pa: P[A], pb: P[B], pc: P[C], pd: P[D], pe: P[E], pf: P[F], pg: P[G], ph: P[H], pi: P[I], pj: P[J], pk: P[K], pl: P[L], pm: P[M], pn: P[N]) extends ParserX[A:+:B:+:C:+:D:+:E:+:F:+:G:+:H:+:I:+:J:+:K:+:L:+:M:+:N:+:HNil]{
  def list = List(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj,pk,pl,pm,pn)
  def >> [T](fn: (A,B,C,D,E,F,G,H,I,J,K,L,M,N)=>T) = new CompoundParser(this, for (a <- pa; b <- pb; c <- pc; d <- pd; e <- pe; f <- pf; g <- pg; h <- ph; i <- pi; j <- pj; k <- pk; l <- pl; m <- pm; n <- pn) yield fn(a,b,c,d,e,f,g,h,i,j,k,l,m,n))
  def ~ [X](px: P[X]) = new Parser15(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj,pk,pl,pm,pn,px)
  def apply (rs: ResultSet) = >>(hlist14)(rs)
}

class Parser15 [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](pa: P[A], pb: P[B], pc: P[C], pd: P[D], pe: P[E], pf: P[F], pg: P[G], ph: P[H], pi: P[I], pj: P[J], pk: P[K], pl: P[L], pm: P[M], pn: P[N], po: P[O]) extends ParserX[A:+:B:+:C:+:D:+:E:+:F:+:G:+:H:+:I:+:J:+:K:+:L:+:M:+:N:+:O:+:HNil]{
  def list = List(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj,pk,pl,pm,pn,po)
  def >> [T](fn: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O)=>T) = new CompoundParser(this, for (a <- pa; b <- pb; c <- pc; d <- pd; e <- pe; f <- pf; g <- pg; h <- ph; i <- pi; j <- pj; k <- pk; l <- pl; m <- pm; n <- pn; o <- po) yield fn(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o))
  def ~ [X](px: P[X]) = new Parser16(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj,pk,pl,pm,pn,po,px)
  def apply (rs: ResultSet) = >>(hlist15)(rs)
}

class Parser16 [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q](pa: P[A], pb: P[B], pc: P[C], pd: P[D], pe: P[E], pf: P[F], pg: P[G], ph: P[H], pi: P[I], pj: P[J], pk: P[K], pl: P[L], pm: P[M], pn: P[N], po: P[O], pq: P[Q]) extends ParserX[A:+:B:+:C:+:D:+:E:+:F:+:G:+:H:+:I:+:J:+:K:+:L:+:M:+:N:+:O:+:Q:+:HNil]{
  def list = List(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj,pk,pl,pm,pn,po,pq)
  def >> [T](fn: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q)=>T) = new CompoundParser(this, for (a <- pa; b <- pb; c <- pc; d <- pd; e <- pe; f <- pf; g <- pg; h <- ph; i <- pi; j <- pj; k <- pk; l <- pl; m <- pm; n <- pn; o <- po; q <- pq) yield fn(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,q))
  def ~ [X](px: P[X]) = new Parser17(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj,pk,pl,pm,pn,po,pq,px)
  def apply (rs: ResultSet) = >>(hlist16)(rs)
}

class Parser17 [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q,R](pa:P[A], pb:P[B], pc:P[C], pd:P[D], pe:P[E], pf:P[F], pg:P[G], ph:P[H], pi:P[I], pj:P[J], pk:P[K], pl:P[L], pm:P[M], pn:P[N], po:P[O], pq:P[Q], pr:P[R]) extends ParserX[A:+:B:+:C:+:D:+:E:+:F:+:G:+:H:+:I:+:J:+:K:+:L:+:M:+:N:+:O:+:Q:+:R:+:HNil]{
  def list = List(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj,pk,pl,pm,pn,po,pq,pr)
  def >> [T](fn: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q,R)=>T) = new CompoundParser(this, for (a<-pa; b<-pb; c<-pc; d<-pd; e<-pe; f<-pf; g<-pg; h<-ph; i<-pi; j<-pj; k<-pk; l<-pl; m<-pm; n<-pn; o<-po; q<-pq; r<-pr) yield fn(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,q,r))
  def ~ [X](px: P[X]) = new Parser18(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj,pk,pl,pm,pn,po,pq,pr,px)
  def apply (rs: ResultSet) = >>(hlist17)(rs)
}

class Parser18 [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q,R,S](pa:P[A], pb:P[B], pc:P[C], pd:P[D], pe:P[E], pf:P[F], pg:P[G], ph:P[H], pi:P[I], pj:P[J], pk:P[K], pl:P[L], pm:P[M], pn:P[N], po:P[O], pq:P[Q], pr:P[R], ps:P[S]) extends ParserX[A:+:B:+:C:+:D:+:E:+:F:+:G:+:H:+:I:+:J:+:K:+:L:+:M:+:N:+:O:+:Q:+:R:+:S:+:HNil]{
  def list = List(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj,pk,pl,pm,pn,po,pq,pr,ps)
  def >> [Z](fn: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q,R,S)=>Z) = new CompoundParser(this, for (a<-pa; b<-pb; c<-pc; d<-pd; e<-pe; f<-pf; g<-pg; h<-ph; i<-pi; j<-pj; k<-pk; l<-pl; m<-pm; n<-pn; o<-po; q<-pq; r<-pr; s<-ps) yield fn(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,q,r,s))
  def ~ [X](px: P[X]) = new Parser19(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj,pk,pl,pm,pn,po,pq,pr,ps,px)
  def apply (rs: ResultSet) = >>(hlist18)(rs)
}

class Parser19 [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q,R,S,T](pa:P[A], pb:P[B], pc:P[C], pd:P[D], pe:P[E], pf:P[F], pg:P[G], ph:P[H], pi:P[I], pj:P[J], pk:P[K], pl:P[L], pm:P[M], pn:P[N], po:P[O], pq:P[Q], pr:P[R], ps:P[S], pt:P[T]) extends ParserX[A:+:B:+:C:+:D:+:E:+:F:+:G:+:H:+:I:+:J:+:K:+:L:+:M:+:N:+:O:+:Q:+:R:+:S:+:T:+:HNil]{
  def list = List(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj,pk,pl,pm,pn,po,pq,pr,ps,pt)
  def >> [Z](fn: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q,R,S,T)=>Z) = new CompoundParser(this, for (a<-pa; b<-pb; c<-pc; d<-pd; e<-pe; f<-pf; g<-pg; h<-ph; i<-pi; j<-pj; k<-pk; l<-pl; m<-pm; n<-pn; o<-po; q<-pq; r<-pr; s<-ps; t<-pt) yield fn(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,q,r,s,t))
  def ~ [X](px: P[X]) = new Parser20(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj,pk,pl,pm,pn,po,pq,pr,ps,pt,px)
  def apply (rs: ResultSet) = >>(hlist19)(rs)
}

class Parser20 [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q,R,S,T,U](pa:P[A], pb:P[B], pc:P[C], pd:P[D], pe:P[E], pf:P[F], pg:P[G], ph:P[H], pi:P[I], pj:P[J], pk:P[K], pl:P[L], pm:P[M], pn:P[N], po:P[O], pq:P[Q], pr:P[R], ps:P[S], pt:P[T], pu:P[U]) extends ParserX[A:+:B:+:C:+:D:+:E:+:F:+:G:+:H:+:I:+:J:+:K:+:L:+:M:+:N:+:O:+:Q:+:R:+:S:+:T:+:U:+:HNil]{
  def list = List(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj,pk,pl,pm,pn,po,pq,pr,ps,pt,pu)
  def >> [Z](fn: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q,R,S,T,U)=>Z) = new CompoundParser(this, for (a<-pa; b<-pb; c<-pc; d<-pd; e<-pe; f<-pf; g<-pg; h<-ph; i<-pi; j<-pj; k<-pk; l<-pl; m<-pm; n<-pn; o<-po; q<-pq; r<-pr; s<-ps; t<-pt; u<-pu) yield fn(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,q,r,s,t,u))
  //def ~ [X](px: P[X]) = new Parser20(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj,pk,pl,pm,po,pq,pr,ps,pt,px)
  def apply (rs: ResultSet) = >>(hlist20)(rs)
}

