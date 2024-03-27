package a

import scala.quoted.*


object O:

  inline def transform[A](inline expr: A): A = ${
    transformImplExpr('expr)
  }

  def pure[A](a:A):A = ???

  def transformImplExpr[A:Type](using Quotes)(expr: Expr[A]): Expr[A] = {
     import quotes.reflect.*
     expr.asTerm match {
         case Inlined(x,y,z) => transformImplExpr(z.asExpr.asInstanceOf[Expr[A]])
         case r@Apply(fun,args) =>  '{
              O.pure(${r.asExpr.asInstanceOf[Expr[A]]}) }
         case other => expr
     }
  }
