//  __..                                                                                   Copyright (c) 2013, Alois Cochard
// (__ |_  _ ._  _.
// .__)[ )(_)[ )(_]
//
// Licensed under the Apache License, Version 2.0

package shona
package entity

import org.specs2.mutable._

import shapeless._

import record._
import graph._
import Property._

class EntityFinderSpec extends Specification {
  val id = Sing("id"); import id._
  val name = Sing("name"); import name._
  val venue = Sing("venue"); import venue._

  val venueV = Vertex[venue.T] ~ (
    int   [id.T], 
    string[name.T]
  )

  val finder = EntityFinder.fromMap(
    venueV, 
    int[id.T],
    Map(
      21 -> Entity((Field[id.T] ~ 21, Field[name.T] ~ "Pizzeria Mario")),
      42 -> Entity((Field[id.T] ~ 42, Field[name.T] ~ "El Tacos"))
    )
  )

  "Shona EntityFinder.fromMap" should {
    "find entities with every fields" in {
      val xs = finder.findAll(venueV.properties, Seq(21, 42)).mapValues(View(_))
      xs(21).id === 21
      xs(21).name === "Pizzeria Mario"
      xs(42).id === 42
      xs(42).name === "El Tacos"
    }

    "find entities for given fields only" in {
      // TODO Once dynamic field selector implemented: prove that name is not part of the entities
      val xs = finder.findAll(string[name.T] :: HNil, Seq(21, 42)).mapValues(View(_))
      xs(21).name === "Pizzeria Mario"
      xs(42).name === "El Tacos"
    }
  }
}


