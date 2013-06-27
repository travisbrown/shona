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

class EntityLoaderSpec extends Specification {
  val id = Sing("id")
  val name = Sing("name")
  val venue = Sing("venue")

  val venueV = Vertex[venue.T] ~ (
    int   [id.T], 
    string[name.T]
  )

  val loader = EntityLoader.fromSeq(venueV, 
    List(
      Entity((Field[id.T] ~ 21, Field[name.T] ~ "Pizzeria Mario")),
      Entity((Field[id.T] ~ 42, Field[name.T] ~ "El Tacos"))
    )
  )

  "Shona EntityLoader.fromSeq" should {
    "load all entities with every fields" in {
      val List(a, b) = loader.all(venueV.properties).map(View(_))
      a.id === 21
      a.name === "Pizzeria Mario"
      b.id === 42
      b.name === "El Tacos"
    }

    "load all entities for given fields only" in {
      // TODO Once dynamic field selector implemented: prove that name is not part of the entities
      val List(a, b) = loader.all(int[id.T] :: HNil).map(View(_))
      a.id === 21
      b.id === 42
    }
  }
}

