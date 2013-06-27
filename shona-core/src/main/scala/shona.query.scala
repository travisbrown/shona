//  __..                                                                                   Copyright (c) 2013, Alois Cochard
// (__ |_  _ ._  _.
// .__)[ )(_)[ )(_]
//
// Licensed under the Apache License, Version 2.0

package shona
package query

import scala.util.parsing.combinator.RegexParsers

import ast._

class Parser extends RegexParsers {
  def apply(input: String): Either[String, Tree] = parseAll(expression, input) match {
    case Success(tree, _) => Right(tree)
    case NoSuccess(message, _) => Left(message)
  }

  def expression: Parser[Tree] = select | operations.map(_(Root))

  def select: Parser[Tree] = (rep1sep(property, ".") ~ operations.?).map {
    case (x :: xs) ~ operations => 
      val tree = xs.foldLeft(Select(Root, x))(Select(_, _))
      operations.fold[Tree](tree)(_(tree))
  }

  def property: Parser[Property] = """\w+""".r.map(Property(_))

  def operations: Parser[Tree => Tree] = rep1(operation).map(xs => (xs: @unchecked) match {
    case x :: xs => tree => xs.foldLeft(Apply(tree, x))(Apply(_, _)) 
  })

  def operation: Parser[Operation] = "[" ~> ("=" ~> mappings).map(MapOperation(_)) <~ "]"

  def mappings: Parser[Seq[Mapping]] = ("{" ~> rep1sep(mapping, ",") <~ "}") | mapping.map(_ :: Nil)

  def mapping: Parser[Mapping] = 
    ("""\w+""".r ~ ":" ~ select).map { case name ~ ":" ~ property => Mapping.Qualified(property, name) } |
    select.map(Mapping.Identity(_))
}

object Parser { def apply(input: String): Either[String, Tree] = (new Parser).apply(input) }

import language.experimental.macros
import scala.reflect.macros.Context

object Query {
  def apply(query: String) = macro QueryMacro.apply
  def parse(query: String): Either[String, ast.Tree] = Parser(query)
}

object QueryMacro extends ReflectionUtils {
  def apply(c: Context)(query: c.Expr[String]): c.Expr[ast.Tree] = {
    import c.universe._

    object Query {
      def fromTree(tree: Tree) = tree match {
        case Literal(Constant(input: String)) => Parser(input) match {
          case Right(query) => query
          case Left(message) => c.abort(tree.pos, message)
        }
        case query => c.abort(query.pos, "The query expression is not a string literal")
      }
    }

    Query.fromTree(query.tree)
    reify(Parser(query.splice).right.get)
  }
}
