//  __..                                                                                   Copyright (c) 2013, Alois Cochard
// (__ |_  _ ._  _.
// .__)[ )(_)[ )(_]
//
// Licensed under the Apache License, Version 2.0

import scala.language.experimental.macros
import scala.reflect.macros.Context

package shona {
  case class Label[T](value: T)

  object Label {
    implicit def witness[A]: Label[A] = macro witness_impl[A]

    def witness_impl[A: c.WeakTypeTag](c: Context): c.Expr[Label[A]] = {
      import c.universe._

      c.weakTypeOf[A].normalize match {
        case t @ ConstantType(Constant(a)) =>
          val instance = Apply(
            TypeApply(
              Select(reify(Label).tree, newTermName("apply")),
              TypeTree(t) :: Nil
            ),
            Literal(Constant(a)) :: Nil
          )

          val typed = Typed(
            instance,
            TypeTree(
              appliedType(
                typeOf[Label[_]].typeConstructor,
                weakTypeOf[A] :: Nil
              )
            )
          )

          c.Expr[Label[A]](instance)

        case _ => c.abort(c.enclosingPosition, "Not a singleton type!")
      }
    }
  }

  trait Sing {
    type T <: Singleton
  }

  object Sing extends ReflectionUtils {
    def apply[A](a: A): Any = macro apply_impl[A]

    def apply_impl[A](c: Context)(a: c.Expr[A]) = {
      import c.universe._

      a.tree match {
        case Literal(constant: Constant) =>
          val anon = newTypeName(c.fresh)
          val wrapper = newTypeName(c.fresh)
          //val label = newTermName(ConstantType(constant).toString)

          c.Expr(
            Block(
              ClassDef(
                Modifiers(),
                anon,
                Nil,
                Template(
                  TypeTree(typeOf[Sing]) :: Nil,
                  emptyValDef,
                  List(
                    constructor(c.universe)(Nil),
                    TypeDef(
                      Modifiers(),
                      newTypeName("T"),
                      Nil,
                      TypeTree(ConstantType(constant))
                    )
                    /*ValDef(
                      Modifiers(Flag.IMPLICIT),
                      label,
                      TypeTree(),
                      Apply(
                        TypeApply(
                          Select(reify(Label).tree, newTermName("apply")),
                          TypeTree(ConstantType(constant)) :: Nil
                        ),
                        Literal(constant) :: Nil
                      )
                    )*/
                  )
                )
              ),
              ClassDef(
                Modifiers(Flag.FINAL),
                wrapper,
                Nil,
                Template(
                  Ident(anon) :: Nil,
                  emptyValDef,
                  constructor(c.universe)(Nil) :: Nil
                )
              ),
              Apply(Select(New(Ident(wrapper)), nme.CONSTRUCTOR), Nil)
            )
          )

        case _ => c.abort(c.enclosingPosition, "Not a literal!")
      }
    }
  }
}

