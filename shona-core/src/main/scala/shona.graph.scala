//  __..                                                                                   Copyright (c) 2013, Alois Cochard
// (__ |_  _ ._  _.
// .__)[ )(_)[ )(_]
//
// Licensed under the Apache License, Version 2.0

package shona

import language.experimental.macros
import scala.reflect.macros.Context

import shapeless.{BasisConstraint, HList, HListerAux, HNil, LUBConstraint}
import shapeless.LUBConstraint._

import record._

package graph {

  class Graph[Vertices <: HList : <<:[AnyVertex]#λ, Edges <: HList : <<:[AnyEdge]#λ](
    val vertices: Vertices,
    val edges: Edges
  ) 

  object Graph {
    def apply[VP <: Product, VL <: HList, EP <: Product, EL <: HList](vp: VP)(ep: EP)(implicit
      vhl: HListerAux[VP, VL], vlub: LUBConstraint[VL, AnyVertex],
      ehl: HListerAux[EP, EL], elub: LUBConstraint[EL, AnyEdge]
    ): Graph[VL, EL] = apply(vhl(vp), ehl(ep))

    def apply[VL <: HList : <<:[AnyVertex]#λ, EL <: HList : <<:[AnyEdge]#λ](vl: VL, el: EL): Graph[VL, EL] = 
      new Graph(vl, el)

    // TODO Investigate implicits based implementation (vs macro)
    def get[G <: AnyGraph](graph: G)(path: String) = macro GraphMacro.get[G]
  }

  object GraphMacro extends ReflectionUtils {
    def get[G <: AnyGraph : c.WeakTypeTag](c: Context)(graph: c.Expr[G])(path: c.Expr[String]) = {
      import c.universe._

      def withLabel(tpe: Type) = {
        val TypeRef(_, _, Label(label) :: _) = tpe
        label -> tpe
      }

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

      object Graph {
        def unapply(tpe: Type) = {
          val TypeRef(_, _, xs) = tpe
          val List(verticesHList, edgesHList) = xs
          val vertices = HList.decons(verticesHList).map(t => withLabel(t))
          val edges = HList.decons(edgesHList).map(t => withLabel(t))
          Some(vertices -> edges)
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

      // TODO Edges support
      val Graph(vertices, _) = c.typeCheck(graph.tree).tpe

      path.tree match {
        case Literal(Constant(vertexLabel: String)) =>
          vertices.map({ case (label, _) => label }).zipWithIndex.find({ case (label, _ ) => label == vertexLabel }) match {
            case Some((_, index)) => c.Expr[Vertex[_, _]](HList.select(index)(Select(c.resetAllAttrs(graph.tree), newTermName("vertices"))))
            case None => c.abort(path.tree.pos, s"Vertex '$vertexLabel' not found")
          }
        case _ => c.abort(path.tree.pos, "Invalid path expression (only vertex labels are currently supported)")
      }
    }
  }

  class Edge[
    N <: String, 
    VFN <: String, VFPL <: HList : <<:[AnyProperty]#λ, PFN <: String, PFT,
    VTN <: String, VTPL <: HList : <<:[AnyProperty]#λ, PTN <: String, PTT
  ](
    name: Label[N],
    from: Vertex[VFN, VFPL], 
    to: Vertex[VTN, VTPL],
    mapping: Mapping[PFN, PFT, PTN, PTT]
  )(implicit
    fromBasis: BasisConstraint[shapeless.::[Property[PFN, PFT], HNil], VFPL],
    toBasis: BasisConstraint[shapeless.::[Property[PTN, PTT], HNil], VTPL]
  )

  object Edge {
    class EdgeBuilder[N <: String](label: Label[N]) { 
      def ~[
        VFN <: String, VFPL <: HList : <<:[AnyProperty]#λ, PFN <: String, PFT,
        VTN <: String, VTPL <: HList : <<:[AnyProperty]#λ, PTN <: String, PTT
      ](
        from: Vertex[VFN, VFPL], 
        to: Vertex[VTN, VTPL],
        mapping: Mapping[PFN, PFT, PTN, PTT]
      )(implicit
        fromBasis: BasisConstraint[shapeless.::[Property[PFN, PFT], HNil], VFPL],
        toBasis: BasisConstraint[shapeless.::[Property[PTN, PTT], HNil], VTPL]
      ) = new Edge(label, from, to, mapping)
    }
    final def apply[N <: String]()(implicit label: Label[N]) = new EdgeBuilder(label)
  }

  class Mapping[FN <: String, FT, TN <: String, TT](from: Property[FN, FT], to: Property[TN, TT], map: FT => TT)

  object Mapping {
    def apply[FN <: String, FT, TN <: String, TT](from: Property[FN, FT], to: Property[TN, TT])(map: FT => TT) = 
      new Mapping(from, to, map)
    def identity[FN <: String, TN <: String, T](from: Property[FN, T], to: Property[TN, T]) = new Mapping(from, to, Predef.identity[T])
  }

  class Property[N <: String, T](override implicit val label: Label[N]) extends Record[N, T]
  object Property {
    def boolean[N <: String]()(implicit label: Label[N]) = new Property[N, Boolean]
    def byte[N <: String]()(implicit label: Label[N]) = new Property[N, Byte]
    def bytes[N <: String]()(implicit label: Label[N]) = new Property[N, Array[Byte]]
    def char[N <: String]()(implicit label: Label[N]) = new Property[N, Array[Char]]
    def double[N <: String]()(implicit label: Label[N]) = new Property[N, Double]
    def float[N <: String]()(implicit label: Label[N]) = new Property[N, Float]
    def int[N <: String]()(implicit label: Label[N]) = new Property[N, Int]
    def long[N <: String]()(implicit label: Label[N]) = new Property[N, Long]
    def short[N <: String]()(implicit label: Label[N]) = new Property[N, Short]
    def string[N <: String]()(implicit label: Label[N]) = new Property[N, String]
  }

  class Vertex[N <: String, Properties <: HList : <<:[AnyProperty]#λ](
    val name: Label[N], 
    val properties: Properties
  )

  object Vertex {
    class VertexBuilder[N <: String](label: Label[N]) { 
      //def ~[PN <: String, T](property: Property[PN, T]) = new Vertex(label, property :: HNil)
      def ~[P <: Product, L <: HList](p: P)(implicit hl: HListerAux[P, L], lub: LUBConstraint[L, AnyProperty]): Vertex[N, L] = 
        new Vertex(label, hl(p))
    }
    final def apply[N <: String]()(implicit label: Label[N]) = new VertexBuilder(label)
  }
}

package object graph {
  type AnyEdge = Edge[_, _, _, _, _, _, _, _, _]
  type AnyGraph = Graph[_, _]
  type AnyProperty = Property[_, _]
  type AnyVertex = Vertex[_, _]
}
