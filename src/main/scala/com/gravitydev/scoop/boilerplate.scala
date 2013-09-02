package com.gravitydev.scoop
package boilerplate

import java.sql.ResultSet

object `package` {
  private [boilerplate] type P[+T] = ResultSetParser[T]
}

abstract class ParserBase [+A] (fn: ResultSet => ParseResult[A]) extends P[A] {
  def ~ [X](px: P[X]) = new Parser2(this, px)
  def apply (rs: ResultSet) = fn(rs)
  def columns: List[query.SelectExprS]
  def >> [T](fn: A=>T): P[T] = new Parser1(this map fn)
  override def toString = "ParserBase(" + util.fnToString(fn) + ")"
}

abstract class ParserX [+T] extends ResultSetParser [T] {
  def list: List[ResultSetParser[_]]
  def columns = list.foldLeft(List[query.SelectExprS]())((l,p) => l ++ p.columns)
  override def toString = list.map(_.toString).mkString(" ~ ")
}

class Parser1 [+A](pa: P[A]) extends ParserX[A] {
  def list = List(pa)
  def >> [T](fn: A=>T) = new Parser1(pa map fn)
  def ~ [X](px: P[X]) = new Parser2(pa,px)
  def apply (rs: ResultSet) = pa(rs)
}

class Parser2 [+A,B](pa: P[A], pb: P[B]) extends ParserX[(A,B)] {
  def list = List(pa,pb)
  def >> [T](fn: (A,B)=>T) = map(fn.tupled)
  def ~ [X](px: P[X]) = new Parser3(pa,pb,px)
  def apply (rs: ResultSet) = (for (a <- pa; b <- pb) yield (a,b))(rs)
}

class Parser3 [+A,B,C](pa: P[A], pb: P[B], pc: P[C]) extends ParserX[(A,B,C)] {
  def list = List(pa,pb,pc)
  def >> [T](fn: (A,B,C)=>T) = map(fn.tupled)
  def ~ [X](px: P[X]) = new Parser4(pa,pb,pc,px)
  def apply (rs: ResultSet) = (for (a<-pa; b<-pb; c<-pc) yield (a,b,c))(rs)
}

class Parser4 [+A,B,C,D](pa: P[A], pb: P[B], pc: P[C], pd: P[D]) extends ParserX[(A,B,C,D)] {
  def list = List(pa,pb,pc,pd)
  def >> [T](fn: (A,B,C,D)=>T) = map(fn.tupled)
  def ~ [X](px: P[X]) = new Parser5(pa,pb,pc,pd,px)
  def apply (rs: ResultSet) = (for (a<-pa; b<-pb; c<-pc; d<-pd) yield (a,b,c,d))(rs)
}

class Parser5 [+A,B,C,D,E](pa: P[A], pb: P[B], pc: P[C], pd: P[D], pe: P[E]) extends ParserX[(A,B,C,D,E)]{
  def list = List(pa,pb,pc,pd,pe)
  def >> [T](fn: (A,B,C,D,E)=>T) = map(fn.tupled)
  def ~ [X](px: P[X]) = new Parser6(pa,pb,pc,pd,pe,px)
  def apply (rs: ResultSet) = (for (a<-pa; b<-pb; c<-pc; d<-pd; e<-pe) yield (a,b,c,d,e))(rs)
}

class Parser6 [+A,B,C,D,E,F](pa: P[A], pb: P[B], pc: P[C], pd: P[D], pe: P[E], pf: P[F]) extends ParserX[(A,B,C,D,E,F)]{
  def list = List(pa,pb,pc,pd,pe,pf)
  def >> [T](fn: (A,B,C,D,E,F)=>T) = map(fn.tupled)
  def ~ [X](px: P[X]) = new Parser7(pa,pb,pc,pd,pe,pf,px)
  def apply (rs: ResultSet) = (for (a<-pa; b<-pb; c<-pc; d<-pd; e<-pe; f<-pf) yield (a,b,c,d,e,f))(rs)
}

class Parser7 [+A,B,C,D,E,F,G](pa: P[A], pb: P[B], pc: P[C], pd: P[D], pe: P[E], pf: P[F], pg: P[G]) extends ParserX[(A,B,C,D,E,F,G)]{
  def list = List(pa,pb,pc,pd,pe,pf,pg)
  def >> [T](fn: (A,B,C,D,E,F,G)=>T) = map(fn.tupled)
  def ~ [X](px: P[X]) = new Parser8(pa,pb,pc,pd,pe,pf,pg,px)
  def apply (rs: ResultSet) = (for (a<-pa; b<-pb; c<-pc; d<-pd; e<-pe; f<-pf; g<-pg) yield (a,b,c,d,e,f,g))(rs)
}

class Parser8 [+A,B,C,D,E,F,G,H](pa: P[A], pb: P[B], pc: P[C], pd: P[D], pe: P[E], pf: P[F], pg: P[G], ph: P[H]) extends ParserX[(A,B,C,D,E,F,G,H)]{
  def list = List(pa,pb,pc,pd,pe,pf,pg,ph)
  def >> [T](fn: (A,B,C,D,E,F,G,H)=>T) = map(fn.tupled)
  def ~ [X](px: P[X]) = new Parser9(pa,pb,pc,pd,pe,pf,pg,ph,px)
  def apply (rs: ResultSet) = (for (a<-pa; b<-pb; c<-pc; d<-pd; e<-pe; f<-pf; g<-pg; h<-ph) yield (a,b,c,d,e,f,g,h))(rs)
}

class Parser9 [+A,B,C,D,E,F,G,H,I](pa: P[A], pb: P[B], pc: P[C], pd: P[D], pe: P[E], pf: P[F], pg: P[G], ph: P[H], pi: P[I]) extends ParserX[(A,B,C,D,E,F,G,H,I)]{
  def list = List(pa,pb,pc,pd,pe,pf,pg,ph,pi)
  def >> [T](fn: (A,B,C,D,E,F,G,H,I)=>T) = map(fn.tupled)
  def ~ [X](px: P[X]) = new Parser10(pa,pb,pc,pd,pe,pf,pg,ph,pi,px)
  def apply (rs: ResultSet) = (for (a<-pa; b<-pb; c<-pc; d<-pd; e<-pe; f<-pf; g<-pg; h<-ph; i<-pi) yield (a,b,c,d,e,f,g,h,i))(rs)
}

class Parser10 [+A,B,C,D,E,F,G,H,I,J](pa: P[A], pb: P[B], pc: P[C], pd: P[D], pe: P[E], pf: P[F], pg: P[G], ph: P[H], pi: P[I], pj: P[J]) extends ParserX[(A,B,C,D,E,F,G,H,I,J)]{
  def list = List(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj)
  def >> [T](fn: (A,B,C,D,E,F,G,H,I,J)=>T) = map(fn.tupled)
  def ~ [X](px: P[X]) = new Parser11(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj,px)
  def apply (rs: ResultSet) = (for (a<-pa; b<-pb; c<-pc; d<-pd; e<-pe; f<-pf; g<-pg; h<-ph; i<-pi; j<-pj) yield (a,b,c,d,e,f,g,h,i,j))(rs)
}

class Parser11 [+A,B,C,D,E,F,G,H,I,J,K](pa: P[A], pb: P[B], pc: P[C], pd: P[D], pe: P[E], pf: P[F], pg: P[G], ph: P[H], pi: P[I], pj: P[J], pk: P[K]) extends ParserX[(A,B,C,D,E,F,G,H,I,J,K)]{
  def list = List(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj,pk)
  def >> [T](fn: (A,B,C,D,E,F,G,H,I,J,K)=>T) = map(fn.tupled)
  def ~ [X](px: P[X]) = new Parser12(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj,pk,px)
  def apply (rs: ResultSet) = (for (a<-pa; b<-pb; c<-pc; d<-pd; e<-pe; f<-pf; g<-pg; h<-ph; i<-pi; j<-pj; k<-pk) yield (a,b,c,d,e,f,g,h,i,j,k))(rs)
}

class Parser12 [+A,B,C,D,E,F,G,H,I,J,K,L](pa: P[A], pb: P[B], pc: P[C], pd: P[D], pe: P[E], pf: P[F], pg: P[G], ph: P[H], pi: P[I], pj: P[J], pk: P[K], pl: P[L]) extends ParserX[(A,B,C,D,E,F,G,H,I,J,K,L)]{
  def list = List(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj,pk,pl)
  def >> [T](fn: (A,B,C,D,E,F,G,H,I,J,K,L)=>T) = map(fn.tupled)
  def ~ [X](px: P[X]) = new Parser13(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj,pk,pl,px)
  def apply (rs: ResultSet) = (for (a<-pa; b<-pb; c<-pc; d<-pd; e<-pe; f<-pf; g<-pg; h<-ph; i<-pi; j<-pj; k<-pk; l<-pl) yield (a,b,c,d,e,f,g,h,i,j,k,l))(rs)
}

class Parser13 [+A,B,C,D,E,F,G,H,I,J,K,L,M](pa: P[A], pb: P[B], pc: P[C], pd: P[D], pe: P[E], pf: P[F], pg: P[G], ph: P[H], pi: P[I], pj: P[J], pk: P[K], pl: P[L], pm: P[M]) extends ParserX[(A,B,C,D,E,F,G,H,I,J,K,L,M)]{
  def list = List(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj,pk,pl,pm)
  def >> [T](fn: (A,B,C,D,E,F,G,H,I,J,K,L,M)=>T) = map(fn.tupled)
  def ~ [X](px: P[X]) = new Parser14(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj,pk,pl,pm,px)
  def apply (rs: ResultSet) = (for (a<-pa; b<-pb; c<-pc; d<-pd; e<-pe; f<-pf; g<-pg; h<-ph; i<-pi; j<-pj; k<-pk; l<-pl; m<-pm) yield (a,b,c,d,e,f,g,h,i,j,k,l,m))(rs)
}

class Parser14 [+A,B,C,D,E,F,G,H,I,J,K,L,M,N](pa: P[A], pb: P[B], pc: P[C], pd: P[D], pe: P[E], pf: P[F], pg: P[G], ph: P[H], pi: P[I], pj: P[J], pk: P[K], pl: P[L], pm: P[M], pn: P[N]) extends ParserX[(A,B,C,D,E,F,G,H,I,J,K,L,M,N)]{
  def list = List(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj,pk,pl,pm,pn)
  def >> [T](fn: (A,B,C,D,E,F,G,H,I,J,K,L,M,N)=>T) = map(fn.tupled)
  def ~ [X](px: P[X]) = new Parser15(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj,pk,pl,pm,pn,px)
  def apply (rs: ResultSet) = (for (a<-pa; b<-pb; c<-pc; d<-pd; e<-pe; f<-pf; g<-pg; h<-ph; i<-pi; j<-pj; k<-pk; l<-pl; m<-pm; n<-pn) yield (a,b,c,d,e,f,g,h,i,j,k,l,m,n))(rs)
}

class Parser15 [+A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](pa: P[A], pb: P[B], pc: P[C], pd: P[D], pe: P[E], pf: P[F], pg: P[G], ph: P[H], pi: P[I], pj: P[J], pk: P[K], pl: P[L], pm: P[M], pn: P[N], po: P[O]) extends ParserX[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O)]{
  def list = List(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj,pk,pl,pm,pn,po)
  def >> [T](fn: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O)=>T) = map(fn.tupled)
  def ~ [X](px: P[X]) = new Parser16(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj,pk,pl,pm,pn,po,px)
  def apply (rs: ResultSet) = (for (a<-pa; b<-pb; c<-pc; d<-pd; e<-pe; f<-pf; g<-pg; h<-ph; i<-pi; j<-pj; k<-pk; l<-pl; m<-pm; n<-pn; o<-po) yield (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o))(rs)
}

class Parser16 [+A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q](pa: P[A], pb: P[B], pc: P[C], pd: P[D], pe: P[E], pf: P[F], pg: P[G], ph: P[H], pi: P[I], pj: P[J], pk: P[K], pl: P[L], pm: P[M], pn: P[N], po: P[O], pq: P[Q]) extends ParserX[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q)]{
  def list = List(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj,pk,pl,pm,pn,po,pq)
  def >> [T](fn: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q)=>T) = map(fn.tupled)
  def ~ [X](px: P[X]) = new Parser17(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj,pk,pl,pm,pn,po,pq,px)
  def apply (rs: ResultSet) = (for (a<-pa; b<-pb; c<-pc; d<-pd; e<-pe; f<-pf; g<-pg; h<-ph; i<-pi; j<-pj; k<-pk; l<-pl; m<-pm; n<-pn; o<-po; q<-pq) yield (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,q))(rs)
}

class Parser17 [+A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q,R](pa:P[A], pb:P[B], pc:P[C], pd:P[D], pe:P[E], pf:P[F], pg:P[G], ph:P[H], pi:P[I], pj:P[J], pk:P[K], pl:P[L], pm:P[M], pn:P[N], po:P[O], pq:P[Q], pr:P[R]) extends ParserX[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q,R)]{
  def list = List(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj,pk,pl,pm,pn,po,pq,pr)
  def >> [T](fn: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q,R)=>T) = map(fn.tupled)
  def ~ [X](px: P[X]) = new Parser18(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj,pk,pl,pm,pn,po,pq,pr,px)
  def apply (rs: ResultSet) = (for (a<-pa; b<-pb; c<-pc; d<-pd; e<-pe; f<-pf; g<-pg; h<-ph; i<-pi; j<-pj; k<-pk; l<-pl; m<-pm; n<-pn; o<-po; q<-pq; r<-pr) yield (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,q,r))(rs)
}

class Parser18 [+A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q,R,S](pa:P[A], pb:P[B], pc:P[C], pd:P[D], pe:P[E], pf:P[F], pg:P[G], ph:P[H], pi:P[I], pj:P[J], pk:P[K], pl:P[L], pm:P[M], pn:P[N], po:P[O], pq:P[Q], pr:P[R], ps:P[S]) extends ParserX[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q,R,S)]{
  def list = List(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj,pk,pl,pm,pn,po,pq,pr,ps)
  def >> [Z](fn: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q,R,S)=>Z) = map(fn.tupled)
  def ~ [X](px: P[X]) = new Parser19(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj,pk,pl,pm,pn,po,pq,pr,ps,px)
  def apply (rs: ResultSet) = (for (a<-pa; b<-pb; c<-pc; d<-pd; e<-pe; f<-pf; g<-pg; h<-ph; i<-pi; j<-pj; k<-pk; l<-pl; m<-pm; n<-pn; o<-po; q<-pq; r<-pr; s<-ps) yield (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,q,r,s))(rs)
}

class Parser19 [+A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q,R,S,T](pa:P[A], pb:P[B], pc:P[C], pd:P[D], pe:P[E], pf:P[F], pg:P[G], ph:P[H], pi:P[I], pj:P[J], pk:P[K], pl:P[L], pm:P[M], pn:P[N], po:P[O], pq:P[Q], pr:P[R], ps:P[S], pt:P[T]) extends ParserX[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q,R,S,T)]{
  def list = List(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj,pk,pl,pm,pn,po,pq,pr,ps,pt)
  def >> [Z](fn: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q,R,S,T)=>Z) = map(fn.tupled)
  def ~ [X](px: P[X]) = new Parser20(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj,pk,pl,pm,pn,po,pq,pr,ps,pt,px)
  def apply (rs: ResultSet) = (for (a<-pa; b<-pb; c<-pc; d<-pd; e<-pe; f<-pf; g<-pg; h<-ph; i<-pi; j<-pj; k<-pk; l<-pl; m<-pm; n<-pn; o<-po; q<-pq; r<-pr; s<-ps; t<-pt) yield (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,q,r,s,t))(rs)
}

class Parser20 [+A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q,R,S,T,U](pa:P[A], pb:P[B], pc:P[C], pd:P[D], pe:P[E], pf:P[F], pg:P[G], ph:P[H], pi:P[I], pj:P[J], pk:P[K], pl:P[L], pm:P[M], pn:P[N], po:P[O], pq:P[Q], pr:P[R], ps:P[S], pt:P[T], pu:P[U]) extends ParserX[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q,R,S,T,U)]{
  def list = List(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj,pk,pl,pm,pn,po,pq,pr,ps,pt,pu)
  def >> [Z](fn: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q,R,S,T,U)=>Z) = map(fn.tupled)
  def ~ [X](px: P[X]) = new Parser21(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj,pk,pl,pm,pn,po,pq,pr,ps,pt,pu,px)
  def apply (rs: ResultSet) = (for (a<-pa; b<-pb; c<-pc; d<-pd; e<-pe; f<-pf; g<-pg; h<-ph; i<-pi; j<-pj; k<-pk; l<-pl; m<-pm; n<-pn; o<-po; q<-pq; r<-pr; s<-ps; t<-pt; u<-pu) yield (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,q,r,s,t,u))(rs)
}

class Parser21 [+A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q,R,S,T,U,V](pa:P[A], pb:P[B], pc:P[C], pd:P[D], pe:P[E], pf:P[F], pg:P[G], ph:P[H], pi:P[I],
  pj:P[J], pk:P[K], pl:P[L], pm:P[M], pn:P[N], po:P[O], pq:P[Q], pr:P[R], ps:P[S], pt:P[T], pu:P[U], pv: P[V]) extends
ParserX[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q,R,S,T,U,V)]{
  def list = List(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj,pk,pl,pm,pn,po,pq,pr,ps,pt,pu,pv)
  def >> [Z](fn: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q,R,S,T,U,V)=>Z) = map(fn.tupled)
  def ~ [X](px: P[X]) = new Parser22(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj,pk,pl,pm,pn,po,pq,pr,ps,pt,pu,pv,px)
  def apply (rs: ResultSet) = (for (a<-pa; b<-pb; c<-pc; d<-pd; e<-pe; f<-pf; g<-pg; h<-ph; i<-pi; j<-pj; k<-pk; l<-pl; m<-pm; n<-pn; o<-po; q<-pq; r<-pr; s<-ps; t<-pt; u<-pu; v<-pv) yield (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,q,r,s,t,u,v))(rs)
}

class Parser22 [+A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q,R,S,T,U,V,W](pa:P[A], pb:P[B], pc:P[C], pd:P[D], pe:P[E], pf:P[F], pg:P[G], ph:P[H],
  pi:P[I], pj:P[J], pk:P[K], pl:P[L], pm:P[M], pn:P[N], po:P[O], pq:P[Q], pr:P[R], ps:P[S], pt:P[T], pu:P[U], pv:P[V], pw:P[W]) extends
ParserX[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q,R,S,T,U,V,W)]{
  def list = List(pa,pb,pc,pd,pe,pf,pg,ph,pi,pj,pk,pl,pm,pn,po,pq,pr,ps,pt,pu,pv,pw)
  def >> [Z](fn: (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q,R,S,T,U,V,W)=>Z) = map(fn.tupled)
  def apply (rs: ResultSet) = (for (a<-pa; b<-pb; c<-pc; d<-pd; e<-pe; f<-pf; g<-pg; h<-ph; i<-pi; j<-pj; k<-pk; l<-pl; m<-pm; n<-pn; o<-po; q<-pq; r<-pr; s<-ps; t<-pt; u<-pu; v<-pv; w<-pw) yield (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,q,r,s,t,u,v,w))(rs)
}

