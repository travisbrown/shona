//  __..                                                                                   Copyright (c) 2013, Alois Cochard
// (__ |_  _ ._  _.
// .__)[ )(_)[ )(_]
//
// Licensed under the Apache License, Version 2.0

package shona

import java.security.MessageDigest
import scala.language.experimental.macros
import scala.reflect.macros.Context

import shapeless.{HList, HListerAux, HNil, LUBConstraint}
import shapeless.LUBConstraint._

import record._

package entity {
  class Entity[Fields <: HList : <<:[Field[_, _]]#λ](val fields: Fields) {
    override def toString = s"Entity(${fields.toString})"
  }

  object Entity {
    def apply[N <: String, T](field: Field[N, T]) = new Entity(field :: HNil)
    def apply[P <: Product, L <: HList](p: P)(implicit hl : HListerAux[P, L], lub: LUBConstraint[L, Field[_, _]]): Entity[L] = 
      new Entity(hl(p))
  }

  class Field[N <: String, T](val label: Label[N], val value: T) extends Record[N, T] {
    override def toString = s"${label.value}(${value})"
  }

  object Field extends RecordFactory[Field] {
    def create[N <: String, T](label: Label[N], value: T) = new Field(label, value)
  }

  class View[E <: Entity[_]](val entity: E)

  object View {
    def apply[E <: Entity[_]](entity: E) = macro ViewMacro.apply[E]
  }

  object ViewMacro extends ReflectionUtils {
    private lazy val messageDigest = MessageDigest.getInstance("MD5")

    def apply[E <: Entity[_] : c.WeakTypeTag](c: Context)(entity: c.Expr[E]): c.Expr[View[E]] = {
      import c._
      import c.universe._

      def digest(x: String): String = messageDigest.digest(x.getBytes).map("%02X".format(_)).mkString

      object Label {
        def unapply(tpe: Type) = {
          val ConstantType(Constant(id: String)) = tpe.normalize
          Some(id)
        }
      }

      object HList {
        def decons(tpe: Type): List[Type] = tpe match {
          case TypeRef(x, _, a :: b :: _) => a :: decons(b)
          case x => Nil
        }

        def select(i: Int): Tree => Tree = tree => i match {
          case 0 => Select(tree, "head")
          case i => select(i - 1)(Select(tree, "tail"))
        }
      }

      object Entity {
        def unapply(tpe: Type) = {
          val TypeRef(_, _, List(fieldsHList)) = tpe
          val fields = HList.decons(fieldsHList).map { x =>
            val TypeRef(_, _, List(Label(label), tpe)) = x
            label -> tpe
          }
          Some(fields)
        }
      }

      val entityType = c.weakTypeOf[E]

      val Entity(fields) = entityType
      val anon = newTypeName(c.fresh)
      val name = newTypeName("View" + digest(fields.toString))

      val accs = fields.zipWithIndex.map { case ((label, _), index) =>
        DefDef(
          Modifiers(),
          newTermName(label),
          Nil,
          Nil,
          TypeTree(),
          Select(
            HList.select(index)(
              Select(
                Ident(newTermName("entity")),
                newTermName("fields")
              )
            ),
            newTermName("value")
          )
        )
      }

      c.Expr[View[E]](
        Block(
          ClassDef(
            Modifiers(),
                anon,
                Nil,
                Template(
                  Select(
                    Select(
                      Ident(newTermName("shona")),
                      newTermName("entity")
                    ),
                    newTypeName("View")
                  ):: Nil,
                  emptyValDef,
                  List(
                    constructor(c.universe)(entity.tree :: Nil),
                    DefDef(
                      Modifiers(Flag.OVERRIDE),
                      newTermName("toString"),
                      Nil,
                      Nil,
                      TypeTree(),
                      Apply(
                        Select(
                          c.literal("View.").tree,
                          newTermName("$plus")
                        ),
                        Select(This(anon), newTermName("entity")) :: Nil
                      )
                    )
                  ) ++ accs
                )
              ),
              ClassDef(
                Modifiers(Flag.FINAL),
                name,
                Nil,
                Template(
                  Ident(anon) :: Nil,
                  emptyValDef,
                  constructor(c.universe)(Nil) :: Nil
                )
              ),
              Apply(Select(New(Ident(name)), nme.CONSTRUCTOR), Nil)
            )
  
        )
    }
  }
}

